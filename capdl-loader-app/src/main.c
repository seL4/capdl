/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <autoconf.h>
#include <capdl_loader_app/gen_config.h>

#include <assert.h>
#include <inttypes.h>
#include <limits.h>

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <sel4platsupport/platsupport.h>
#include <cpio/cpio.h>
#include <simple-default/simple-default.h>

#include <vka/kobject_t.h>
#include <utils/util.h>
#include <sel4/sel4.h>
#include <sel4utils/sel4_zf_logif.h>
#include "capdl.h"

#ifdef CONFIG_DEBUG_BUILD
#include <utils/attribute.h>
#include <muslcsys/vsyscall.h>
#endif

#include "capdl_spec.h"

#ifdef CONFIG_ARCH_ARM
#include <capdl_loader_app/platform_info.h>
#endif

#ifdef CONFIG_ARCH_RISCV
#define seL4_PageDirIndexBits seL4_PageTableIndexBits
#define PT_LEVEL_SLOT(vaddr, level) (((vaddr) >> ((seL4_PageTableIndexBits * (level-1)) + seL4_PageBits)) & MASK(seL4_PageTableIndexBits))
#endif

#define PML4_SLOT(vaddr) ((vaddr >> (seL4_PDPTIndexBits + seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PML4IndexBits))
#define PDPT_SLOT(vaddr) ((vaddr >> (seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PDPTIndexBits))
#define PD_SLOT(vaddr)   ((vaddr >> (seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PageDirIndexBits))
#define PT_SLOT(vaddr)   ((vaddr >> seL4_PageBits) & MASK(seL4_PageTableIndexBits))
#define PGD_SLOT(vaddr) ((vaddr >> (seL4_PUDIndexBits + seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PGDIndexBits))
#define PUD_SLOT(vaddr) ((vaddr >> (seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PUDIndexBits))

#define CAPDL_SHARED_FRAMES

#define STACK_ALIGNMENT_BYTES 16

#define MAX_STREAM_IDS 60

static seL4_CPtr capdl_to_sel4_orig[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_copy[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_irq[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sched_ctrl[CONFIG_MAX_NUM_NODES];
/* For static allocation, this maps from untyped derivation index to cslot.
 * Otherwise, this stores the result of sort_untypeds. */
static seL4_CPtr untyped_cptrs[CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS];

static seL4_CPtr free_slot_start, free_slot_end;

static seL4_CPtr first_arm_iospace;

// Hack for seL4_TCB_WriteRegisters because we can't take the address of local variables.
static seL4_UserContext global_user_context;

extern char _capdl_archive[];
extern char _capdl_archive_end[];

/* This symbol is provided by the GNU linker and points at the start/end of our
 * ELF image.
 */
extern char __executable_start[];
extern char _end[];

/* A region at which to map destination frames during loading
 * This is expected to be initialised by 'init_copy_addr' on system init */
uintptr_t copy_addr;

uint32_t sid_number = 0;
/* In the case where we just want a 4K page and we cannot allocate
 * a page table ourselves, we use this pre allocated region that
 * is guaranteed to have a pagetable */
/* Make this slot large enough for 64KiB frames which is the largest
 * last-level page size across all architectures that we support. */
static char copy_addr_with_pt[PAGE_SIZE_4K * 16] __attribute__((aligned(PAGE_SIZE_4K * 16)));

static seL4_BootInfoHeader *extended_bootinfo_table[SEL4_BOOTINFO_HEADER_NUM] = {0};

/* helper functions ---------------------------------------------------------------------------- */

static seL4_CPtr get_free_slot(void)
{
    return free_slot_start;
}

static void next_free_slot(void)
{
    free_slot_start++;
    ZF_LOGF_IF(free_slot_start >= free_slot_end, "Ran out of free slots!");
}

typedef enum {MOVE, COPY} init_cnode_mode;
typedef enum {ORIG, DUP, IRQ, SCHED_CTRL} seL4_cap_type;

static seL4_CPtr orig_caps(CDL_ObjID object_id)
{
    assert(object_id < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_orig[object_id];
}

static seL4_CPtr dup_caps(CDL_ObjID object_id)
{
    assert(object_id < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_copy[object_id];
}

static seL4_CPtr irq_caps(CDL_IRQ irq)
{
    assert(irq < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_irq[irq];
}

static seL4_CPtr sched_ctrl_caps(CDL_Core id)
{
    assert(id < CONFIG_MAX_NUM_NODES);
    return capdl_to_sched_ctrl[id];
}

static void add_sel4_cap(CDL_ObjID object_id, seL4_cap_type type, seL4_CPtr slot)
{
    if (type == ORIG) {
        capdl_to_sel4_orig[object_id] = slot;
    } else if (type == DUP) {
        capdl_to_sel4_copy[object_id] = slot;
    } else if (type == IRQ) {
        capdl_to_sel4_irq[object_id] = slot;
    } else if (type == SCHED_CTRL) {
        capdl_to_sched_ctrl[object_id] = slot;
    }
}

static CDL_Object
*get_spec_object(CDL_Model *spec, CDL_ObjID object_id)
{
    return &spec->objects[object_id];
}

static seL4_Word get_capData(CDL_CapData d)
{
    switch (d.tag) {
    case CDL_CapData_Badge:
        return d.badge;
    case CDL_CapData_Guard:
        return seL4_CNode_CapData_new(d.guard_bits, d.guard_size).words[0];
    case CDL_CapData_Raw:
        return d.data;
    default:
        ZF_LOGF("invalid cap data");
        return seL4_NilData;
    }
}

static CDL_Cap *get_cap_at(CDL_Object *obj, unsigned int slot)
{
    for (unsigned int i = 0; i < CDL_Obj_NumSlots(obj); i++) {
        CDL_CapSlot *s = CDL_Obj_GetSlot(obj, i);
        if (CDL_CapSlot_Slot(s) == slot) {
            return CDL_CapSlot_Cap(s);
        }
    }

    /* Not found. */
    return NULL;
}

#ifdef CONFIG_ARCH_X86_64
static CDL_Cap *get_cdl_frame_pdpt(CDL_ObjID root, uintptr_t vaddr, CDL_Model *spec)
{
    CDL_Object *cdl_pml4 = get_spec_object(spec, root);
    CDL_Cap *pdpt_cap = get_cap_at(cdl_pml4, PML4_SLOT(vaddr));
    if (pdpt_cap == NULL) {
        ZF_LOGF("Could not find PD cap %s[%d]", CDL_Obj_Name(cdl_pml4), (int)PML4_SLOT(vaddr));
    }
    return pdpt_cap;
}

static CDL_Cap *get_cdl_frame_pd(CDL_ObjID root, uintptr_t vaddr, CDL_Model *spec)
{
    CDL_Cap *pdpt_cap = get_cdl_frame_pdpt(root, vaddr, spec);
    CDL_Object *cdl_pdpt = get_spec_object(spec, CDL_Cap_ObjID(pdpt_cap));
    CDL_Cap *pd_cap = get_cap_at(cdl_pdpt, PDPT_SLOT(vaddr));
    if (pd_cap == NULL) {
        ZF_LOGF("Could not find PD cap %s[%d]", CDL_Obj_Name(cdl_pdpt), (int)PDPT_SLOT(vaddr));
    }
    return pd_cap;
}
#endif

#ifdef CONFIG_ARCH_AARCH64
static CDL_Cap *get_cdl_frame_pud(CDL_ObjID root, uintptr_t vaddr, CDL_Model *spec)
{
    CDL_Object *cdl_pgd = get_spec_object(spec, root);
    CDL_Cap *pud_cap = get_cap_at(cdl_pgd, PGD_SLOT(vaddr));
    if (pud_cap == NULL) {
        ZF_LOGF("Could not find PUD cap %s[%d]", CDL_Obj_Name(cdl_pgd), (int)PGD_SLOT(vaddr));
    }
    return pud_cap;
}

static CDL_Cap *get_cdl_frame_pd(CDL_ObjID root, uintptr_t vaddr, CDL_Model *spec)
{
#if CDL_PT_NUM_LEVELS == 3
    CDL_Object *cdl_pud = get_spec_object(spec, root);
#else
    CDL_Cap *pud_cap = get_cdl_frame_pud(root, vaddr, spec);
    CDL_Object *cdl_pud = get_spec_object(spec, CDL_Cap_ObjID(pud_cap));
#endif
    CDL_Cap *pd_cap = get_cap_at(cdl_pud, PUD_SLOT(vaddr));
    if (pd_cap == NULL) {
        ZF_LOGF("Could not find PD cap %s[%d]", CDL_Obj_Name(cdl_pud), (int)PUD_SLOT(vaddr));
    }
    return pd_cap;
}
#endif

#ifndef CONFIG_ARCH_RISCV
static CDL_Cap *get_cdl_frame_pt(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
#if defined(CONFIG_ARCH_X86_64) || defined(CONFIG_ARCH_AARCH64)
    CDL_Cap *pd_cap = get_cdl_frame_pd(pd, vaddr, spec);
    CDL_Object *cdl_pd = get_spec_object(spec, CDL_Cap_ObjID(pd_cap));
#else
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
#endif
    CDL_Cap *pt_cap = get_cap_at(cdl_pd, PD_SLOT(vaddr));
    if (pt_cap == NULL) {
        ZF_LOGF("Could not find PT cap %s[%d]", CDL_Obj_Name(cdl_pd), (int)PD_SLOT(vaddr));
    }
    return pt_cap;
}

#else /* CONFIG_ARCH_RISCV */

/**
 * Do a recursive traversal from the top to bottom of a page table structure to
 * get the cap for a particular page table object for a certain vaddr at a certain
 * level. The level variable treats level==CONFIG_PT_LEVELS as the root page table
 * object, and level 0 as the bottom level 4k frames.
 */
static CDL_Cap *get_cdl_frame_pt_recurse(CDL_ObjID root, uintptr_t vaddr, CDL_Model *spec, int level)
{
    CDL_Object *cdl_pt = NULL;
    if (level < CONFIG_PT_LEVELS) {
        CDL_Cap *pt_cap = get_cdl_frame_pt_recurse(root, vaddr, spec, level + 1);
        cdl_pt = get_spec_object(spec, CDL_Cap_ObjID(pt_cap));
    } else {
        cdl_pt = get_spec_object(spec, root);
    }
    CDL_Cap *pt_cap_ret = get_cap_at(cdl_pt, PT_LEVEL_SLOT(vaddr, level));
    if (pt_cap_ret == NULL) {
        ZF_LOGF("Could not find PD cap %s[%d]", CDL_Obj_Name(cdl_pt), (int)PT_LEVEL_SLOT(vaddr, level));
    }
    return pt_cap_ret;
}

static CDL_Cap *get_cdl_frame_pt(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
    return get_cdl_frame_pt_recurse(pd, vaddr, spec, 2);
}

#endif

static CDL_Cap *get_cdl_frame_cap(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
    CDL_Cap *pt_cap = get_cdl_frame_pt(pd, vaddr, spec);

    /* Check if the PT cap is actually a large frame cap. */
    if (pt_cap->type == CDL_FrameCap) {
        return pt_cap;
    }

    CDL_Object *cdl_pt = get_spec_object(spec, CDL_Cap_ObjID(pt_cap));
    CDL_Cap *frame_cap = get_cap_at(cdl_pt, PT_SLOT(vaddr));
    if (frame_cap == NULL) {
        ZF_LOGF("Could not find frame cap %s[%d]", CDL_Obj_Name(cdl_pt), (int)PT_SLOT(vaddr));
    }

    return frame_cap;
}

/* elf file loading hack - prefill objects with the data defined in the elf file */
static seL4_CPtr get_frame_cap(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
    return orig_caps(CDL_Cap_ObjID(get_cdl_frame_cap(pd, vaddr, spec)));
}

void init_copy_frame(seL4_BootInfo *bootinfo)
{
    /* An original frame will be mapped, backing copy_addr_with_pt. For
     * correctness we should unmap this before mapping into this
     * address. We locate the frame cap by looking in boot info
     * and knowing that the userImageFrames are ordered by virtual
     * address in our address space. The flush is probably not
     * required, but doesn't hurt to be cautious.
     */

    /* Find the number of frames in the user image according to
     * bootinfo, and compare that to the number of frames backing
     * the image computed by comparing start and end symbols. If
     * these numbers are different, assume the image was padded
     * to the left. */
    unsigned int num_user_image_frames_reported =
        bootinfo->userImageFrames.end - bootinfo->userImageFrames.start;
    unsigned int num_user_image_frames_measured =
        (ROUND_UP((uintptr_t)&_end, PAGE_SIZE_4K) -
         (uintptr_t)&__executable_start) / PAGE_SIZE_4K;

    if (num_user_image_frames_reported < num_user_image_frames_measured) {
        ZF_LOGE("Too few frames caps in bootinfo to back user image");
        return;
    }

    size_t additional_user_image_bytes =
        (num_user_image_frames_reported - num_user_image_frames_measured) * PAGE_SIZE_4K;

    if (additional_user_image_bytes > (uintptr_t)&__executable_start) {
        ZF_LOGE("User image padding too high to fit before start symbol");
        return;
    }

    uintptr_t lowest_mapped_vaddr =
        (uintptr_t)&__executable_start - additional_user_image_bytes;

    seL4_CPtr copy_addr_frame = bootinfo->userImageFrames.start +
                                ((uintptr_t)copy_addr_with_pt) / PAGE_SIZE_4K -
                                lowest_mapped_vaddr / PAGE_SIZE_4K;
    /* We currently will assume that we are on a 32-bit platform
     * that has a single PD, followed by all the PTs. So to find
     * our PT in the paging objects list we just need to add 1
     * to skip the PD */
    seL4_CPtr copy_addr_pt = bootinfo->userImagePaging.start + 1 +
                             PD_SLOT(((uintptr_t)copy_addr)) - PD_SLOT(((uintptr_t)&__executable_start));
#if defined(CONFIG_ARCH_X86_64) || defined(CONFIG_ARCH_AARCH64)
    /* guess that there is one PDPT and PML4 on x86_64 or one PGD and PUD on aarch64 */
    copy_addr_pt += 2;
#endif
#ifdef CONFIG_ARCH_RISCV
    /* The base case assumes that there is 2 levels paging structure and already skips
     * the top level and level after that.  We then also need to skip the remaining levels */
    copy_addr_pt += CONFIG_PT_LEVELS - 2;
#endif
    int error;

    for (int i = 0; i < sizeof(copy_addr_with_pt) / PAGE_SIZE_4K; i++) {
        error = seL4_ARCH_Page_Unmap(copy_addr_frame + i);
        ZF_LOGF_IFERR(error, "");
    }
}

#if !CONFIG_CAPDL_LOADER_STATIC_ALLOC
/* Sort the untyped objects from largest to smallest.
 * This ensures that fragmentation is eliminated if the objects
 * themselves are also sorted, largest to smallest.
 *
 * Sorting done using counting sort.
 */
static unsigned int sort_untypeds(seL4_BootInfo *bootinfo)
{
    seL4_CPtr untyped_start = bootinfo->untyped.start;
    seL4_CPtr untyped_end = bootinfo->untyped.end;

    ZF_LOGD("Sorting untypeds...");

    seL4_Word count[CONFIG_WORD_SIZE] = {0};

    // Count how many untypeds there are of each size.
    for (seL4_Word untyped_index = 0; untyped_index != untyped_end - untyped_start; untyped_index++) {
        if (!bootinfo->untypedList[untyped_index].isDevice) {
            count[bootinfo->untypedList[untyped_index].sizeBits] += 1;
        }
    }

    // Calculate the starting index for each untyped.
    seL4_Word total = 0;
    for (seL4_Word size = CONFIG_WORD_SIZE - 1; size != 0; size--) {
        seL4_Word oldCount = count[size];
        count[size] = total;
        total += oldCount;
    }

    unsigned int num_normal_untypes = 0;

    // Store untypeds in untyped_cptrs array.
    for (seL4_Word untyped_index = 0; untyped_index != untyped_end - untyped_start; untyped_index++) {
        if (bootinfo->untypedList[untyped_index].isDevice) {
            ZF_LOGD("Untyped %3d (cptr=%p) (addr=%p) is of size %2d. Skipping as it is device",
                    untyped_index, (void *)(untyped_start + untyped_index),
                    (void *)bootinfo->untypedList[untyped_index].paddr,
                    bootinfo->untypedList[untyped_index].sizeBits);
        } else {
            ZF_LOGD("Untyped %3d (cptr=%p) (addr=%p) is of size %2d. Placing in slot %d...",
                    untyped_index, (void *)(untyped_start + untyped_index),
                    (void *)bootinfo->untypedList[untyped_index].paddr,
                    bootinfo->untypedList[untyped_index].sizeBits,
                    count[bootinfo->untypedList[untyped_index].sizeBits]);

            untyped_cptrs[count[bootinfo->untypedList[untyped_index].sizeBits]] = untyped_start +  untyped_index;
            count[bootinfo->untypedList[untyped_index].sizeBits] += 1;
            num_normal_untypes++;
        }
    }

    return num_normal_untypes;
}
#endif /* !CONFIG_CAPDL_LOADER_STATIC_ALLOC */

static void parse_bootinfo(seL4_BootInfo *bootinfo, CDL_Model *spec)
{
    ZF_LOGD("Parsing bootinfo...");

    free_slot_start = bootinfo->empty.start;
    free_slot_end = bootinfo->empty.end;

    /* When using libsel4platsupport for printing support, we end up using some
     * of our free slots during serial port initialisation. Skip over these to
     * avoid failing our own allocations. Note, this value is just hardcoded
     * for the amount of slots this initialisation currently uses up.
     * JIRA: CAMKES-204.
     */
    free_slot_start += 16;

    ZF_LOGD("  %ld free cap slots, from %ld to %ld",
            (long)(free_slot_end - free_slot_start),
            (long)free_slot_start,
            (long)free_slot_end);

    /* We need to be able to actual store caps to the maximum number of objects
     * we may be dealing with.
     * This check can still pass and initialisation fail as we need extra slots
     * for duplicates for CNodes and TCBs.
     */
    assert(free_slot_end - free_slot_start >= CONFIG_CAPDL_LOADER_MAX_OBJECTS);


#if CONFIG_CAPDL_LOADER_STATIC_ALLOC
    /*
     * Make sure the untypeds in the model correspond to what we got
     * from bootinfo.
     */
    int bi_start = 0;
    for (int u = 0; u < spec->num_untyped; u++) {
        bool found = false;
        int num_untyped = bootinfo->untyped.end - bootinfo->untyped.start;
        CDL_Object *ut = &spec->objects[spec->untyped[u].untyped];
        assert(CDL_Obj_Type(ut) == CDL_Untyped);

        for (int i = bi_start; i < num_untyped; i++) {
            seL4_Word ut_paddr = bootinfo->untypedList[i].paddr;
            if (bootinfo->untypedList[i].paddr == ut->paddr) {
                seL4_Uint8 ut_size = bootinfo->untypedList[i].sizeBits;
                ZF_LOGF_IF(ut_size != ut->size_bits,
                           "Ut at %p in incorrect size, expected %u got %u",
                           ut->paddr, ut->size_bits, ut_size);
                untyped_cptrs[u] = bootinfo->untyped.start + i;
                found = true;
                if (i == bi_start) {
                    bi_start++;
                }
            }
        }
        ZF_LOGF_IF(!found, "Failed to find ut for %p", ut->paddr);
    }
#else
    /* Probably an inconsistency in the build configuration, so fail now. */
    ZF_LOGF_IF(spec->num_untyped, "spec has static alloc, but loader is compiled for dynamic");
#endif

#if CONFIG_CAPDL_LOADER_PRINT_UNTYPEDS
    int num_untyped = bootinfo->untyped.end - bootinfo->untyped.start;
    ZF_LOGD("  Untyped memory (%d)", num_untyped);
    for (int i = 0; i < num_untyped; i++) {
        uintptr_t ut_paddr = bootinfo->untypedList[i].paddr;
        uintptr_t ut_size = bootinfo->untypedList[i].sizeBits;
        bool ut_isDevice = bootinfo->untypedList[i].isDevice;
        ZF_LOGD("    0x%016" PRIxPTR " - 0x%016" PRIxPTR " (%s)", ut_paddr,
                ut_paddr + BIT(ut_size), ut_isDevice ? "device" : "memory");
    }
#endif

    ZF_LOGD("Loader is running in domain %d", bootinfo->initThreadDomain);

    first_arm_iospace = bootinfo->ioSpaceCaps.start;
}

#if !CONFIG_CAPDL_LOADER_STATIC_ALLOC
static int find_device_object(seL4_Word paddr, seL4_Word type, int size_bits, seL4_CPtr free_slot,
                              CDL_ObjID obj_id, seL4_BootInfo *bootinfo, CDL_Model *spec)
{
    int error;
    seL4_CPtr hold_slot = 0;
    /* See if an overlapping object was already created, can only do this for frames.
     * Any overlapping object will be the previous one, since objects are created in
     * order of physical address */
    if (type != seL4_UntypedObject && obj_id > 0) {
        CDL_ObjID prev = obj_id - 1;
        CDL_Object *obj = &spec->objects[prev];
        if (CDL_Obj_Type(obj) == CDL_Frame &&
            obj->frame_extra.paddr == paddr &&
            CDL_Obj_SizeBits(obj) == size_bits) {
            /* Attempt to copy the cap */
            error = seL4_CNode_Copy(seL4_CapInitThreadCNode, free_slot, CONFIG_WORD_SIZE,
                                    seL4_CapInitThreadCNode, orig_caps(prev), CONFIG_WORD_SIZE, seL4_AllRights);
            ZF_LOGF_IFERR(error, "");
            return 0;
        }
    }
    /* Assume we are allocating from a device untyped. Do a linear search for it */
    for (unsigned int i = 0; i < bootinfo->untyped.end - bootinfo->untyped.start; i++) {
        if (bootinfo->untypedList[i].paddr <= (uintptr_t)paddr &&
            bootinfo->untypedList[i].paddr + BIT(bootinfo->untypedList[i].sizeBits) - 1 >= (uintptr_t)paddr + BIT(size_bits) - 1) {
            /* just allocate objects until we get the one we want. To do this
             * correctly we cannot just destroy the cap we allocate, since
             * if it's the only frame from the untyped this will reset the
             * freeIndex in the kernel, resulting in the next allocation
             * giving the same object. To prevent this we need to hold
             * a single allocation to (lock) the untyped, allowing us to
             * allocate and delete over the rest of the untyped. In order
             * to get a free slot we assume that the slot immediately after
             * us is not yet allocated. We'll give it back though :) */
            while (1) {
                error = seL4_Untyped_Retype(bootinfo->untyped.start + i, type, size_bits,
                                            seL4_CapInitThreadCNode, 0, 0, free_slot, 1);
                ZF_LOGF_IFERR(error, "");
                seL4_ARCH_Page_GetAddress_t addr;
                if (type == seL4_UntypedObject) {
                    /* if it's an untyped, create a temporary frame in it
                     * to get the address from */
                    error = seL4_Untyped_Retype(free_slot, arch_kobject_get_type(KOBJECT_FRAME, seL4_PageBits), seL4_PageBits,
                                                seL4_CapInitThreadCNode, 0, 0, free_slot + 2, 1);
                    ZF_LOGF_IFERR(error, "");
                    addr = seL4_ARCH_Page_GetAddress(free_slot + 2);
                    error = seL4_CNode_Delete(seL4_CapInitThreadCNode, free_slot + 2, CONFIG_WORD_SIZE);
                    ZF_LOGF_IFERR(error, "");
                } else {
                    addr = seL4_ARCH_Page_GetAddress(free_slot);
                }
                ZF_LOGF_IFERR(addr.error, "Could not get address for untyped cap.");
                if (addr.paddr == (uintptr_t)paddr) {
                    /* nailed it */
                    /* delete any holding cap */
                    if (hold_slot) {
                        error = seL4_CNode_Delete(seL4_CapInitThreadCNode, hold_slot, CONFIG_WORD_SIZE);
                        ZF_LOGF_IFERR(error, "");
                    }
                    return 0;
                }
                ZF_LOGF_IF(addr.paddr > (uintptr_t)paddr, "device frames probably not ordered by physical address");

                /* if we are currently using a hold slot we can just delete the cap, otherwise start the hold */
                if (hold_slot) {
                    error = seL4_CNode_Delete(seL4_CapInitThreadCNode, free_slot, CONFIG_WORD_SIZE);
                    ZF_LOGF_IFERR(error, "");
                } else {
                    hold_slot = free_slot + 1;
                    error = seL4_CNode_Move(seL4_CapInitThreadCNode, hold_slot, CONFIG_WORD_SIZE, seL4_CapInitThreadCNode, free_slot,
                                            CONFIG_WORD_SIZE);
                    ZF_LOGF_IFERR(error, "");
                }
            }
        }
    }
    return -1;
}

bool isDeviceObject(CDL_Object *obj)
{
    return CDL_Obj_Paddr(obj) != 0;
}
#endif /* !CONFIG_CAPDL_LOADER_STATIC_ALLOC */

/* Create objects */
static int retype_untyped(seL4_CPtr free_slot, seL4_CPtr free_untyped,
                          seL4_ArchObjectType object_type, int object_size)
{
    seL4_CPtr root = seL4_CapInitThreadCNode;
    int node_index = 0;
    int node_depth = 0;
    int node_offset = free_slot;

    int no_objects = 1;

    ZF_LOGF_IF(object_type >= seL4_ObjectTypeCount,
               "Invalid object type %zu size %zu",
               (size_t) object_type, (size_t) object_size);

    return seL4_Untyped_Retype(free_untyped, object_type, object_size,
                               root, node_index, node_depth, node_offset, no_objects);

}

unsigned int create_object(CDL_Model *spec, CDL_Object *obj, CDL_ObjID id, seL4_BootInfo *info, seL4_CPtr untyped_slot,
                           unsigned int free_slot)
{
    int obj_size = CDL_Obj_SizeBits(obj);
    seL4_ArchObjectType obj_type;

    switch (CDL_Obj_Type(obj)) {
    case CDL_Frame:
        obj_type = kobject_get_type(KOBJECT_FRAME, obj_size);
        break;
    case CDL_ASIDPool:
        obj_type = CDL_Untyped;
        obj_size = seL4_ASIDPoolBits;
        break;
#ifdef CONFIG_KERNEL_MCS
    case CDL_SchedContext:
        obj_size = kobject_get_size(KOBJECT_SCHED_CONTEXT, obj_size);
        obj_type = (seL4_ArchObjectType) CDL_Obj_Type(obj);
        break;
#endif
    default:
        obj_type = (seL4_ArchObjectType) CDL_Obj_Type(obj);
    }

    ZF_LOGD_IF(CDL_Obj_Type(obj) == CDL_CNode, " (CNode of size %d bits)", obj_size);

    seL4_Error err = seL4_NoError;

#ifdef CONFIG_ARCH_X86
    if (CDL_Obj_Type(obj) == CDL_IOPorts) {
        err = seL4_X86_IOPortControl_Issue(seL4_CapIOPortControl, obj->start, obj->end, seL4_CapInitThreadCNode, free_slot,
                                           CONFIG_WORD_SIZE);
        ZF_LOGF_IF(err != seL4_NoError, "Failed to allocate IOPort for range [%d,%d]", (int)obj->start, (int)obj->end);
        return seL4_NoError;
    }
#endif

    /* There can be multiple sids per context bank, currently only 1 sid per cb is implemented for the vms.
     * When this gets extended we need to decide to add sid number -> cb number map into the haskell / python tool
     * or generate the capdl spec so that the order remains correct here e.g a list a stream ids followed by the cb they
     * are mapped to, the cb condition here (1***) will the reset the stream id number back to 0 for the next context bank.
     */
#ifdef CONFIG_ARM_SMMU
    if (CDL_Obj_Type(obj) == CDL_SID) {
        err = seL4_ARM_SIDControl_GetSID(seL4_CapSMMUSIDControl, sid_number, seL4_CapInitThreadCNode, free_slot,
                                         CONFIG_WORD_SIZE);
        ZF_LOGF_IF(err != seL4_NoError, "Failed to allocate SID cap");
        sid_number++;
        ZF_LOGF_IF(sid_number > MAX_STREAM_IDS, "Stream ID numbers exhausted");
        return seL4_NoError;
    } else if (CDL_Obj_Type(obj) == CDL_CB) {
        err = seL4_ARM_CBControl_GetCB(seL4_CapSMMUCBControl, CDL_CB_Bank(obj), seL4_CapInitThreadCNode, free_slot,
                                       CONFIG_WORD_SIZE);
        ZF_LOGF_IF(err != seL4_NoError, "Failed to allocate CB cap");
        sid_number = 0; //(1***)
        return seL4_NoError;
    }
#endif

#if !CONFIG_CAPDL_LOADER_STATIC_ALLOC
    if (isDeviceObject(obj)) {
        seL4_Word paddr = CDL_Obj_Paddr(obj);
        ZF_LOGD(" device frame/untyped, paddr = %p, size = %d bits", (void *) paddr, obj_size);

        /* This is a device object. Look for it in bootinfo. */
        err = find_device_object(paddr, obj_type, obj_size, free_slot, id, info, spec);
        ZF_LOGF_IF(err != seL4_NoError, "Failed to find device frame/untyped at paddr = %p", (void *) paddr);
        return seL4_NoError;
    }
#endif

    /* It's not a device object, or it's a statically allocated device
     * object, so we don't need to search for it. */
    return retype_untyped(free_slot, untyped_slot, obj_type, obj_size);
}

static int requires_creation(CDL_ObjectType type)
{
    switch (type) {
    case CDL_Interrupt:
#ifdef CONFIG_ARCH_X86
    case CDL_IODevice:
    case CDL_IOAPICInterrupt:
    case CDL_MSIInterrupt:
#endif /* CONFIG_ARCH_X86 */
#ifdef CONFIG_ARCH_ARM
    case CDL_ARMIODevice:
    case CDL_ARMInterrupt:
#endif /* CONFIG_ARCH_ARM */
        return false;
    default:
        return true;
    }
}

#if CONFIG_CAPDL_LOADER_STATIC_ALLOC

/*
 * Spec was statically allocated; just run its untyped derivation steps.
 */
static void create_objects(CDL_Model *spec, seL4_BootInfo *bootinfo)
{
    ZF_LOGD("Creating objects...");

    unsigned int free_slot_index = 0;

    /* First, allocate most objects and update the spec database with
       the cslot locations. The exception is ASIDPools, where
       create_object only allocates the backing untypeds. */
    for (int ut_index = 0; ut_index < spec->num_untyped; ut_index++) {
        CDL_UntypedDerivation *ud = &spec->untyped[ut_index];
        seL4_CPtr untyped_cptr = untyped_cptrs[ut_index];
        for (int child = 0; child < ud->num; child++) {
            CDL_ObjID obj_id = ud->children[child];
            seL4_CPtr free_slot = free_slot_start + free_slot_index;
            CDL_Object *obj = &spec->objects[obj_id];
            CDL_ObjectType capdl_obj_type = CDL_Obj_Type(obj);

            ZF_LOGV("Creating object %s in slot %ld, from untyped %lx...",
                    CDL_Obj_Name(obj), (long)free_slot, (long)untyped_cptr);

            ZF_LOGF_IF(!requires_creation(capdl_obj_type),
                       "object %s is in static allocation, but requires_creation is false",
                       CDL_Obj_Name(obj));
            seL4_Error err = create_object(spec, obj, obj_id, bootinfo, untyped_cptr, free_slot);
            if (err == seL4_NoError) {
                add_sel4_cap(obj_id, ORIG, free_slot);
                free_slot_index++;
            } else {
                /* Exit with failure. */
                ZF_LOGF_IFERR(err, "Untyped retype failed with unexpected error");
            }
        }
    }

    /* Now, we turn the backing untypeds into ASID pools, in the order
       given by the ASID slot allocation policy. This fixes the layout
       inside the kernel's ASID table, which ensures consistency with
       verification models. */
    if (spec->num_asid_slots > 1) {
        ZF_LOGD("Creating ASID pools...");
    }
    for (seL4_Word asid_high = 1; asid_high < spec->num_asid_slots; asid_high++) {
        CDL_ObjID obj_id = spec->asid_slots[asid_high];
        seL4_CPtr asidpool_ut = orig_caps(obj_id);
        seL4_CPtr asidpool_slot = free_slot_start + free_slot_index;

        seL4_Error err = seL4_ARCH_ASIDControl_MakePool(seL4_CapASIDControl, asidpool_ut,
                                                        seL4_CapInitThreadCNode, asidpool_slot,
                                                        CONFIG_WORD_SIZE);
        ZF_LOGF_IFERR(err, "Failed to create ASID pool #%d from ut slot %ld into slot %ld",
                      (int)asid_high, (long)asidpool_ut, (long)asidpool_slot);

        // update to point to our new ASID pool
        add_sel4_cap(obj_id, ORIG, asidpool_slot);
        free_slot_index++;
    }

    // Update the free slot to go past all the objects we just made.
    free_slot_start += free_slot_index;
}

#else /* !CONFIG_CAPDL_LOADER_STATIC_ALLOC */

/*
 * Spec was not statically allocated; run a simple allocator.
 *
 * For best results, this relies on capDL-tool grouping device objects
 * by address and sorting other objects from largest to smallest, to
 * minimise memory fragmentation. See CapDL/PrintC.hs.
 */
static void create_objects(CDL_Model *spec, seL4_BootInfo *bootinfo)
{
    /* Sort untypeds from largest to smallest. */
    unsigned int num_normal_untypes = sort_untypeds(bootinfo);

    ZF_LOGD("Creating objects...");

    /* First, allocate most objects and update the spec database with
       the cslot locations. The exception is ASIDPools, where
       create_object only allocates the backing untypeds. */
    unsigned int obj_id_index = 0;
    unsigned int free_slot_index = 0;
    unsigned int ut_index = 0;

    // Each time through the loop either:
    //  - we successfully create an object, and move to the next object to create
    //    OR
    //  - we fail to create an object, and move to the next untyped object

    while (obj_id_index < spec->num && ut_index < num_normal_untypes) {
        CDL_ObjID obj_id = obj_id_index;
        seL4_CPtr free_slot = free_slot_start + free_slot_index;
        seL4_CPtr untyped_cptr = untyped_cptrs[ut_index];
        CDL_Object *obj = &spec->objects[obj_id_index];
        CDL_ObjectType capdl_obj_type = CDL_Obj_Type(obj);

        ZF_LOGV("Creating object %s in slot %ld, from untyped %lx...", CDL_Obj_Name(obj), (long)free_slot,
                (long)untyped_cptr);

        if (requires_creation(capdl_obj_type)) {
            /* at this point we are definitely creating an object - figure out what it is */
            seL4_Error err = create_object(spec, obj, obj_id, bootinfo, untyped_cptr, free_slot);
            if (err == seL4_NoError) {
                add_sel4_cap(obj_id, ORIG, free_slot);
                free_slot_index++;
            } else if (err == seL4_NotEnoughMemory) {
                /* go to the next untyped to allocate objects - this one is empty */
                ut_index++;
                /* we failed to process the current object, go back 1 */
                obj_id_index--;
            } else {
                /* Exit with failure. */
                ZF_LOGF_IFERR(err, "Untyped retype failed with unexpected error");
            }
        }
        obj_id_index++;
    }

    if (obj_id_index != spec->num) {
        /* We didn't iterate through all the objects. */
        ZF_LOGF("Ran out of untyped memory while creating objects.");
    }

    /* Now, we turn the backing untypeds into ASID pools, in the order
       given by the ASID slot allocation policy. This fixes the layout
       inside the kernel's ASID table, which ensures consistency with
       verification models. */
    if (spec->num_asid_slots > 1) {
        ZF_LOGD("Creating ASID pools...");
    }
    for (seL4_Word asid_high = 1; asid_high < spec->num_asid_slots; asid_high++) {
        CDL_ObjID obj_id = spec->asid_slots[asid_high];
        seL4_CPtr asid_ut = orig_caps(obj_id);
        seL4_CPtr asid_slot = free_slot_start + free_slot_index;

        seL4_Error err = seL4_ARCH_ASIDControl_MakePool(seL4_CapASIDControl, asid_ut,
                                                        seL4_CapInitThreadCNode, asid_slot,
                                                        CONFIG_WORD_SIZE);
        ZF_LOGF_IFERR(err, "Failed to create asid pool");

        // update to point to our new ASID pool
        add_sel4_cap(obj_id, ORIG, asid_slot);
        free_slot_index++;
    }

    // Update the free slot to go past all the objects we just made.
    free_slot_start += free_slot_index;
}

#endif /* !CONFIG_CAPDL_LOADER_STATIC_ALLOC */

static void create_irq_cap(CDL_IRQ irq, CDL_Object *obj, seL4_CPtr free_slot)
{
    seL4_CPtr root = seL4_CapInitThreadCNode;
    int index = free_slot;
    int depth = CONFIG_WORD_SIZE;
    int error;

    switch (CDL_Obj_Type(obj)) {
#if defined(CONFIG_ARCH_X86)
    case CDL_IOAPICInterrupt:
        error = seL4_IRQControl_GetIOAPIC(seL4_CapIRQControl, root, index, depth,
                                          obj->ioapicirq_extra.ioapic, obj->ioapicirq_extra.ioapic_pin,
                                          obj->ioapicirq_extra.level, obj->ioapicirq_extra.polarity,
                                          irq);
        break;
    case CDL_MSIInterrupt:
        error = seL4_IRQControl_GetMSI(seL4_CapIRQControl, root, index, depth,
                                       obj->msiirq_extra.pci_bus, obj->msiirq_extra.pci_dev,
                                       obj->msiirq_extra.pci_fun, obj->msiirq_extra.handle, irq);
        break;
#elif defined(CONFIG_ARCH_ARM)
    case CDL_ARMInterrupt:
#if CONFIG_MAX_NUM_NODES > 1
        error = seL4_IRQControl_GetTriggerCore(seL4_CapIRQControl, irq, obj->armirq_extra.trigger,
                                               root, index, depth, obj->armirq_extra.target);
#else
        error = seL4_IRQControl_GetTrigger(seL4_CapIRQControl, irq, obj->armirq_extra.trigger,
                                           root, index, depth);
#endif
        break;
#endif
    default:
        error = seL4_IRQControl_Get(seL4_CapIRQControl, irq, root, index, depth);
    }
    ZF_LOGF_IFERR(error, "Failed to create irq cap");
    add_sel4_cap(irq, IRQ, index);
}

static void create_irq_caps(CDL_Model *spec)
{
    ZF_LOGD("Creating irq handler caps...");

    for (CDL_IRQ irq = 0; irq < spec->num_irqs; irq++) {
        if (spec->irqs[irq] != INVALID_OBJ_ID) {
            seL4_CPtr free_slot = get_free_slot();

            ZF_LOGD(" Creating irq handler cap for IRQ %d...", irq);
            create_irq_cap(irq, &spec->objects[spec->irqs[irq]], free_slot);
            next_free_slot();
        }
    }
}

/* Mint a cap that will not be given to the user */
/* Used for badging interrupt notifications and, in the RT kernel, fault eps */
static void mint_cap(CDL_ObjID object_id, int free_slot, seL4_Word badge, seL4_CapRights_t rights)
{
    seL4_CPtr dest_root = seL4_CapInitThreadCNode;
    int dest_index = free_slot;
    int dest_depth = CONFIG_WORD_SIZE;

    seL4_CPtr src_root = seL4_CapInitThreadCNode;
    int src_index = orig_caps(object_id);
    int src_depth = CONFIG_WORD_SIZE;

    int error = seL4_CNode_Mint(dest_root, dest_index, dest_depth,
                                src_root, src_index, src_depth, rights,
                                badge);
    ZF_LOGF_IF(error, "Failed to mint cap");
}

/* Duplicate capabilities */
static void duplicate_cap(CDL_ObjID object_id, int free_slot)
{
    seL4_CapRights_t rights = seL4_AllRights;

    seL4_CPtr dest_root = seL4_CapInitThreadCNode;
    int dest_index = free_slot;
    int dest_depth = CONFIG_WORD_SIZE;

    seL4_CPtr src_root = seL4_CapInitThreadCNode;
    int src_index = orig_caps(object_id);
    int src_depth = CONFIG_WORD_SIZE;

    int error = seL4_CNode_Copy(dest_root, dest_index, dest_depth,
                                src_root, src_index, src_depth, rights);
    ZF_LOGF_IFERR(error, "");

    add_sel4_cap(object_id, DUP, dest_index);
}

static void duplicate_caps(CDL_Model *spec)
{
    ZF_LOGD("Duplicating CNodes...");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode || spec->objects[obj_id].type == CDL_TCB) {
            ZF_LOGD(" Duplicating %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            int free_slot = get_free_slot();
            duplicate_cap(obj_id, free_slot);
            next_free_slot();
        }
    }
}

static void create_sched_ctrl_caps(seL4_BootInfo *bi)
{
#ifdef CONFIG_KERNEL_MCS
    for (seL4_Word i = 0; i <= bi->schedcontrol.end - bi->schedcontrol.start; i++) {
        add_sel4_cap(i, SCHED_CTRL, i + bi->schedcontrol.start);
    }
#endif
}

/* Initialise SCs */
static void init_sc(CDL_Model *spec, CDL_ObjID sc, CDL_Core affinity)
{
    CDL_Object *cdl_sc = get_spec_object(spec, sc);

    uint64_t UNUSED budget = CDL_SC_Budget(cdl_sc);
    uint64_t UNUSED period = CDL_SC_Period(cdl_sc);
    seL4_Word UNUSED data = CDL_SC_Data(cdl_sc);

    ZF_LOGD("budget: %llu, period: %llu, data: %u", budget, period, data);

    seL4_CPtr UNUSED seL4_sc = orig_caps(sc);
    seL4_CPtr UNUSED sched_control = sched_ctrl_caps(affinity);
#ifdef CONFIG_KERNEL_MCS
    /* Assign the sched context to run on the CPU that the root task runs on. */
    int error = seL4_SchedControl_Configure(sched_control,
                                            seL4_sc, budget, period, 0, data);
    ZF_LOGF_IFERR(error, "");
#endif
}

/* Initialise TCBs */
static void init_tcb(CDL_Model *spec, CDL_ObjID tcb)
{
    CDL_Object *cdl_tcb = get_spec_object(spec, tcb);

    CDL_Cap *cdl_cspace_root = get_cap_at(cdl_tcb, CDL_TCB_CTable_Slot);
    if (cdl_cspace_root == NULL) {
        ZF_LOGD("Could not find CSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }
    CDL_Cap *cdl_vspace_root = get_cap_at(cdl_tcb, CDL_TCB_VTable_Slot);
    if (cdl_vspace_root == NULL) {
        ZF_LOGD("Could not find VSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }
    CDL_Cap *cdl_ipcbuffer   = get_cap_at(cdl_tcb, CDL_TCB_IPCBuffer_Slot);
    if (cdl_ipcbuffer == NULL) {
        ZF_LOGD("  Warning: TCB has no IPC buffer");
    }
#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) || defined(CONFIG_VTX)
    CDL_Cap *cdl_vcpu = get_cap_at(cdl_tcb, CDL_TCB_VCPU_SLOT);
#endif

    CDL_Cap *cdl_sc   = get_cap_at(cdl_tcb, CDL_TCB_SC_Slot);

    seL4_Word ipcbuffer_addr = CDL_TCB_IPCBuffer_Addr(cdl_tcb);
    uint8_t priority = CDL_TCB_Priority(cdl_tcb);
    CDL_Core UNUSED affinity = CDL_TCB_Affinity(cdl_tcb);
    uint8_t UNUSED max_priority = CDL_TCB_MaxPriority(cdl_tcb);

    seL4_CPtr sel4_tcb = orig_caps(tcb);

    seL4_CPtr sel4_cspace_root = cdl_cspace_root == NULL ? 0 : orig_caps(CDL_Cap_ObjID(cdl_cspace_root));
    seL4_CPtr sel4_vspace_root = cdl_vspace_root ? orig_caps(CDL_Cap_ObjID(cdl_vspace_root)) : 0;
    seL4_CPtr sel4_ipcbuffer   = cdl_ipcbuffer ? orig_caps(CDL_Cap_ObjID(cdl_ipcbuffer)) : 0;
    seL4_CPtr UNUSED sel4_sc   = cdl_sc ? orig_caps(CDL_Cap_ObjID(cdl_sc)) : 0;
#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) || defined(CONFIG_VTX)
    seL4_CPtr sel4_vcpu        = cdl_vcpu ? orig_caps(CDL_Cap_ObjID(cdl_vcpu)) : 0;
#endif

    seL4_CPtr sel4_fault_ep;
    seL4_CPtr UNUSED sel4_tempfault_ep;
    seL4_CPtr badged_sel4_fault_ep;

    if (config_set(CONFIG_KERNEL_MCS)) {
        /* Fault ep cptrs are in the caller's cspace */

        CDL_Cap *cdl_fault_ep   = get_cap_at(cdl_tcb, CDL_TCB_FaultEP_Slot);
        if (cdl_fault_ep == NULL) {
            ZF_LOGW("  Warning: TCB has no fault endpoint");
        }

        CDL_Cap *cdl_tempfault_ep   = get_cap_at(cdl_tcb, CDL_TCB_TemporalFaultEP_Slot);
        if (cdl_tempfault_ep == NULL) {
            ZF_LOGW("  Warning: TCB has no temporal fault endpoint");
        }

        sel4_fault_ep = cdl_fault_ep ? orig_caps(CDL_Cap_ObjID(cdl_fault_ep)) : 0;
        sel4_tempfault_ep = cdl_tempfault_ep ? orig_caps(CDL_Cap_ObjID(cdl_tempfault_ep)) : 0;

        if (sel4_fault_ep != 0) {
            seL4_Word fault_ep_badge = get_capData(CDL_Cap_Data(cdl_fault_ep));
            seL4_CapRights_t fault_ep_rights = CDL_seL4_Cap_Rights(cdl_fault_ep);
            if (fault_ep_badge != 0 || !seL4_CapRights_get_capAllowAllRights(fault_ep_rights)) {
                badged_sel4_fault_ep = (seL4_CPtr) get_free_slot();
                mint_cap(CDL_Cap_ObjID(cdl_fault_ep), badged_sel4_fault_ep,
                         fault_ep_badge, fault_ep_rights);
                next_free_slot();
                sel4_fault_ep = badged_sel4_fault_ep;

            }
        }
    } else {
        /* Fault ep cptrs are in the configured thread's cspace */
        sel4_fault_ep = cdl_tcb->tcb_extra.fault_ep;
    }

    seL4_Word sel4_cspace_root_data = seL4_NilData;
    seL4_Word sel4_vspace_root_data = seL4_NilData;
    if (cdl_cspace_root != NULL) {
        sel4_cspace_root_data = get_capData(CDL_Cap_Data(cdl_cspace_root));
    }
    if (cdl_vspace_root != NULL) {
        sel4_vspace_root_data = get_capData(CDL_Cap_Data(cdl_vspace_root));
    }

    /*
     * seL4_TCB_Configure requires a valid CSpace, VSpace and IPC buffer cap to
     * succeed at assigning any of them. We first try and perform seL4_TCB_Configure
     * but if any of these objects are missing we fall back to only trying to assign
     * an IPC buffer if we have one using seL4_TCB_SetIPCBuffer.  We print an error
     * if a CSpace is available but a VSpace is not or if there is a VSpace but no CSpace.
     */
    int error;
#ifdef CONFIG_KERNEL_MCS
    if (sel4_sc) {
        init_sc(spec, CDL_Cap_ObjID(cdl_sc), affinity);
    }

    if (cdl_cspace_root && cdl_vspace_root && sel4_ipcbuffer) {
        error = seL4_TCB_Configure(sel4_tcb,
                                   sel4_cspace_root, sel4_cspace_root_data,
                                   sel4_vspace_root, sel4_vspace_root_data,
                                   ipcbuffer_addr, sel4_ipcbuffer);
        ZF_LOGF_IFERR(error, "");
    } else {
        ZF_LOGE_IFERR(cdl_cspace_root || cdl_vspace_root || sel4_ipcbuffer,
                      "Could not call seL4_TCB_Configure as not all required objects provided: "
                      "VSpace: %p, CSpace: %p, IPC Buffer: %"SEL4_PRIx_word, cdl_vspace_root, cdl_cspace_root, sel4_ipcbuffer);

        if (sel4_ipcbuffer) {
            error = seL4_TCB_SetIPCBuffer(sel4_tcb, ipcbuffer_addr, sel4_ipcbuffer);
            ZF_LOGF_IFERR(error, "");
        }
    }

    error = seL4_TCB_SetSchedParams(sel4_tcb, seL4_CapInitThreadTCB, max_priority, priority,
                                    sel4_sc, sel4_fault_ep);
    ZF_LOGF_IFERR(error, "");

    error = seL4_TCB_SetTimeoutEndpoint(sel4_tcb, sel4_tempfault_ep);
#else
    if (cdl_cspace_root && cdl_vspace_root && sel4_ipcbuffer) {
        error = seL4_TCB_Configure(sel4_tcb, sel4_fault_ep,
                                   sel4_cspace_root, sel4_cspace_root_data,
                                   sel4_vspace_root, sel4_vspace_root_data,
                                   ipcbuffer_addr, sel4_ipcbuffer);
        ZF_LOGF_IFERR(error, "");
    } else {
        ZF_LOGE_IFERR(cdl_cspace_root || cdl_vspace_root || sel4_ipcbuffer,
                      "Could not call seL4_TCB_Configure as not all required objects provided: "
                      "VSpace: %p, CSpace: %p, IPC Buffer: %"SEL4_PRIx_word, cdl_vspace_root, cdl_cspace_root, sel4_ipcbuffer);

        if (sel4_ipcbuffer) {
            error = seL4_TCB_SetIPCBuffer(sel4_tcb, ipcbuffer_addr, sel4_ipcbuffer);
            ZF_LOGF_IFERR(error, "");
        }
    }

    error = seL4_TCB_SetSchedParams(sel4_tcb, seL4_CapInitThreadTCB, max_priority, priority);

    ZF_LOGF_IFERR(error, "");

#if CONFIG_MAX_NUM_NODES > 1
    error = seL4_TCB_SetAffinity(sel4_tcb, affinity);
#endif

#endif

    ZF_LOGF_IFERR(error, "");

#if defined(CONFIG_ARM_HYPERVISOR_SUPPORT) || defined(CONFIG_VTX)
    if (sel4_vcpu) {
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
        int error = seL4_ARM_VCPU_SetTCB(sel4_vcpu, sel4_tcb);
#else //CONFIG_VTX
        int error = seL4_X86_VCPU_SetTCB(sel4_vcpu, sel4_tcb);
#endif
        ZF_LOGF_IFERR(error, "Failed to bind TCB %s to VCPU %s",
                      CDL_Obj_Name(cdl_tcb), CDL_Obj_Name(get_spec_object(spec, CDL_Cap_ObjID(cdl_vcpu))));
    }
#endif

#ifdef CONFIG_DEBUG_BUILD
    /* Name the thread after its TCB name if possible. We need to do some
     * juggling first to ensure the name will not overflow the IPC buffer.
     */
    char safe_name[seL4_MsgMaxLength * sizeof(seL4_Word)];
    const char *name = CDL_Obj_Name(cdl_tcb);
    if (name != NULL) {
        strncpy(safe_name, name, sizeof(safe_name) - 1);
        safe_name[sizeof(safe_name) - 1] = '\0';
        (void)seL4_DebugNameThread(sel4_tcb, safe_name);
    }
#endif
}

static void configure_tcb(CDL_Model *spec, CDL_ObjID tcb)
{
    seL4_CPtr sel4_tcb = dup_caps(tcb);

    CDL_Object *cdl_tcb = get_spec_object(spec, tcb);
    const seL4_Word *argv = cdl_tcb->tcb_extra.init;
    int argc = cdl_tcb->tcb_extra.init_sz;

    uintptr_t pc = CDL_TCB_PC(cdl_tcb);
    uintptr_t sp = CDL_TCB_SP(cdl_tcb);

    if (sp % (sizeof(uintptr_t) * 2) != 0) {
        ZF_LOGF("TCB %s's stack pointer is not dword-aligned", CDL_Obj_Name(&spec->objects[tcb]));
    }
    int reg_args = 0;
#if defined(CONFIG_ARCH_ARM)
    /* On ARM, the first four arguments go in registers. */
    reg_args = 4;
#endif
#if defined(CONFIG_ARCH_IA32) && defined(CONFIG_CAPDL_LOADER_CC_REGISTERS)
    reg_args = 4;
#endif
#if defined(CONFIG_ARCH_X86_64)
    reg_args = 4;
#endif
#if defined(CONFIG_ARCH_RISCV)
    reg_args = 4;
#endif

    if (argc > reg_args) {
#ifdef CONFIG_CAPDL_LOADER_CC_REGISTERS
        ZF_LOGF("TCB %s has more than four arguments, which is not supported using"
                " the register calling convention", CDL_Obj_Name(&spec->objects[tcb]));
#else //!CONFIG_CAPDL_LOADER_CC_REGISTERS
        /* We need to map the TCB's stack into our address space because there
         * are arguments to write.
         */

        /* Find the TCB's PD. */
        CDL_Cap *cdl_vspace_root = get_cap_at(cdl_tcb, CDL_TCB_VTable_Slot);
        CDL_ObjID pd = CDL_Cap_ObjID(cdl_vspace_root);

        if (STACK_ALIGNMENT_BYTES % sizeof(*argv)) {
            ZF_LOGF("Stack alignment requirement not evenly divisible by argument size");
        }

        /* The stack pointer of new threads will initially be aligned to
         * STACK_ALIGNMENT_BYTES bytes. Any padding required to enforce
         * this alignment will come before any stack arguments.
         */

        unsigned int num_stack_args = argc - reg_args; // positive because argc > reg_args
        unsigned int args_per_alignment = (STACK_ALIGNMENT_BYTES / sizeof(*argv));
        unsigned int num_unaligned_args = num_stack_args % args_per_alignment;

        if (num_unaligned_args != 0) {
            unsigned int num_padding_args = args_per_alignment - num_unaligned_args;
            unsigned int num_padding_bytes = num_padding_args * sizeof(*argv);
            sp -= num_padding_bytes;
        }

        /* Find and map the frame representing the TCB's stack. Note that we do
         * `sp - sizeof(uintptr_t)` because the stack pointer may be on a page
         * boundary.
         */
        seL4_CPtr frame = get_frame_cap(pd, sp - sizeof(uintptr_t), spec);
        /* FIXME: The above could actually fail messily if the user has given a
         * spec with stack pointers that point outside the ELF image.
         */
        seL4_ARCH_VMAttributes attribs = seL4_ARCH_Default_VMAttributes;
#ifdef CONFIG_ARCH_ARM
        attribs |= seL4_ARM_ExecuteNever;
#endif
        int error = seL4_ARCH_Page_Map(frame, seL4_CapInitThreadPD, (seL4_Word)copy_addr_with_pt,
                                       seL4_ReadWrite, attribs);
        ZF_LOGF_IFERR(error, "");

        /* Write all necessary arguments to the TCB's stack. */
        for (int i = argc - 1; i >= 0 && i >= reg_args; i--) {
            if (i != argc - 1 && sp % PAGE_SIZE_4K == 0) {
                /* We could support this case with more complicated logic, but
                 * choose not to.
                 */
                ZF_LOGF("TCB %s's initial arguments cause its stack to cross a page boundary",
                        CDL_Obj_Name(&spec->objects[tcb]));
            }
            sp -= sizeof(seL4_Word);
            *(seL4_Word *)(copy_addr_with_pt + sp % PAGE_SIZE_4K) = argv[i];
        }

#ifdef CONFIG_ARCH_ARM
        error = seL4_ARM_Page_Unify_Instruction(frame, 0, PAGE_SIZE_4K);
        ZF_LOGF_IFERR(error, "");
#endif //CONFIG_ARCH_ARM
        error = seL4_ARCH_Page_Unmap(frame);
        ZF_LOGF_IFERR(error, "");
#endif //CONFIG_CAPDL_LOADER_CC_REGISTERS
    }

    seL4_UserContext regs = {
#if defined(CONFIG_ARCH_ARM)
        .pc = pc,
        .sp = sp,
#ifdef CONFIG_ARCH_AARCH32
        .r0 = argc > 0 ? argv[0] : 0,
        .r1 = argc > 1 ? argv[1] : 0,
        .r2 = argc > 2 ? argv[2] : 0,
        .r3 = argc > 3 ? argv[3] : 0,
#else // CONFIG_ARCH_AARCH64
        .x0 = argc > 0 ? argv[0] : 0,
        .x1 = argc > 1 ? argv[1] : 0,
        .x2 = argc > 2 ? argv[2] : 0,
        .x3 = argc > 3 ? argv[3] : 0,
#endif // CONFIG_ARCH_AARCH32
#elif defined(CONFIG_ARCH_IA32)
        .eip = pc,
        .esp = sp,
#ifdef CONFIG_CAPDL_LOADER_CC_REGISTERS
        .eax = argc > 2 ? argv[2] : 0,
        .ebx = argc > 3 ? argv[3] : 0,
        .ecx = argc > 0 ? argv[0] : 0,
        .edx = argc > 1 ? argv[1] : 0,
#endif
#elif defined(CONFIG_ARCH_X86_64)
        .rip = pc,
        .rsp = sp,
        .rdi = argc > 0 ? argv[0] : 0,
        .rsi = argc > 1 ? argv[1] : 0,
        .rdx = argc > 2 ? argv[2] : 0,
        .rcx = argc > 3 ? argv[3] : 0,
#elif defined(CONFIG_ARCH_RISCV)
        .pc = pc,
        .sp = sp,
        .a0 = argc > 0 ? argv[0] : 0,
        .a1 = argc > 1 ? argv[1] : 0,
        .a2 = argc > 2 ? argv[2] : 0,
        .a3 = argc > 3 ? argv[3] : 0,
#endif
    };
    ZF_LOGD("  Setting up _start()");
    ZF_LOGD("   pc   = %p", (void *)pc);
    ZF_LOGD("   sp   = %p", (void *)sp);
    for (int i = 0; i < argc; i++) {
        ZF_LOGD("   arg%d = %p", i, (void *)argv[i]);
    }

    global_user_context = regs;

    int error = seL4_TCB_WriteRegisters(sel4_tcb, false, 0,
                                        sizeof(seL4_UserContext) / sizeof(seL4_Word),
                                        &global_user_context);
    ZF_LOGF_IFERR(error, "");

    uint32_t UNUSED domain = CDL_TCB_Domain(cdl_tcb);
    ZF_LOGD("  Assigning thread to domain %u...", domain);
    error = seL4_DomainSet_Set(seL4_CapDomain, domain, sel4_tcb);
    ZF_LOGF_IFERR(error, "");
}

static void init_tcbs(CDL_Model *spec)
{
    ZF_LOGD("Initialising TCBs...");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            ZF_LOGD(" Initialising %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            init_tcb(spec, obj_id);

            ZF_LOGD(" Configuring %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            configure_tcb(spec, obj_id);
        }
    }
}


static void init_irq(CDL_Model *spec, CDL_IRQ irq_no)
{
    seL4_CPtr irq_handler_cap = irq_caps(irq_no);

    CDL_Object *cdl_irq = get_spec_object(spec, spec->irqs[irq_no]);
    assert(cdl_irq != NULL);

#ifdef CONFIG_ARCH_X86
    assert(cdl_irq->type == CDL_Interrupt || cdl_irq->type == CDL_IOAPICInterrupt || cdl_irq->type == CDL_MSIInterrupt);
#elif CONFIG_ARCH_ARM
    assert(cdl_irq->type == CDL_Interrupt || cdl_irq->type == CDL_ARMInterrupt);
#else
    assert(cdl_irq->type == CDL_Interrupt);
#endif

    if (cdl_irq->size_bits != 0) {
        ZF_LOGF("Misconfigured IRQ; an IRQ must have a size of 0.");
    }
    if (cdl_irq->slots.num > 1) {
        ZF_LOGF("Misconfigured IRQ; an IRQ cannot have more than one assigned endpoint.");
    }

    if (cdl_irq->slots.num == 1) {
        /* This IRQ is bound. */
        CDL_Cap *endpoint_cap = &cdl_irq->slots.slot[0].cap;
        seL4_CPtr endpoint_cptr;

        seL4_Word badge = get_capData(CDL_Cap_Data(endpoint_cap));
        if (badge) {
            endpoint_cptr = (seL4_CPtr)get_free_slot();
            mint_cap(CDL_Cap_ObjID(endpoint_cap), endpoint_cptr, badge, seL4_AllRights);
            next_free_slot();
        } else {
            endpoint_cptr = orig_caps(CDL_Cap_ObjID(endpoint_cap));
        }

        int error = seL4_IRQHandler_SetNotification(irq_handler_cap, endpoint_cptr);
        ZF_LOGF_IFERR(error, "");
    }
}

static void init_irqs(CDL_Model *spec)
{
    ZF_LOGD("Initialising IRQ handler caps...");

    for (CDL_IRQ irq = 0; irq < spec->num_irqs; irq++) {
        if (spec->irqs[irq] != INVALID_OBJ_ID) {
            ZF_LOGD(" Initialising handler for IRQ %d...", irq);
            init_irq(spec, irq);
        }
    }
}

/* Initialise virtual address spaces */
static void set_asid(CDL_Model *spec UNUSED, CDL_ObjID page)
{
    seL4_CPtr sel4_page = orig_caps(page);
    int error = seL4_ARCH_ASIDPool_Assign(seL4_CapInitThreadASIDPool, sel4_page);
    ZF_LOGF_IFERR(error, "");
}

static void init_pd_asids(CDL_Model *spec)
{
    ZF_LOGD("Initialising Page Directory ASIDs...");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        CDL_ObjectType type = CDL_TOP_LEVEL_PD;
        if (spec->objects[obj_id].type == type) {
            ZF_LOGD(" Initialising pd/pml4 ASID %s...",
                    CDL_Obj_Name(&spec->objects[obj_id]));
            set_asid(spec, obj_id);
        }
    }
}

static void map_page(CDL_Model *spec UNUSED, CDL_Cap *page_cap, CDL_ObjID pd_id,
                     seL4_CapRights_t rights, seL4_Word vaddr)
{
    CDL_ObjID page = CDL_Cap_ObjID(page_cap);

    // TODO: We should not be using the original cap here
    seL4_CPtr sel4_page = orig_caps(page);
    seL4_CPtr sel4_pd = orig_caps(pd_id);
#ifdef CONFIG_CAPDL_LOADER_WRITEABLE_PAGES
    /* Make instruction pages writeable to support software breakpoints */
    if (seL4_CapRights_get_capAllowGrant(rights)) {
        rights = seL4_CapRights_set_capAllowWrite(rights, true);
    }
#endif
    seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(page_cap);
    ZF_LOGD("   Mapping %s into %s with rights={G: %d, R: %d, W: %d}, vaddr=0x%x, vm_attribs=0x%x",
            CDL_Obj_Name(&spec->objects[page]),
            CDL_Obj_Name(&spec->objects[pd_id]),
            seL4_CapRights_get_capAllowGrant(rights),
            seL4_CapRights_get_capAllowRead(rights),
            seL4_CapRights_get_capAllowWrite(rights),
            vaddr, vm_attribs);

    if (CDL_Cap_Type(page_cap) == CDL_PTCap) {
        int error = seL4_ARCH_PageTable_Map(sel4_page, sel4_pd, vaddr, vm_attribs);
        ZF_LOGF_IFERR(error, "");

    } else if (CDL_Cap_Type(page_cap) == CDL_FrameCap) {
#ifdef CAPDL_SHARED_FRAMES
        /* hack to support shared frames: create a new cap for each mapping */
        int dest_index = get_free_slot();

        int error_0 = seL4_CNode_Copy(seL4_CapInitThreadCNode, dest_index, CONFIG_WORD_SIZE,
                                      seL4_CapInitThreadCNode, sel4_page, CONFIG_WORD_SIZE, seL4_AllRights);
        ZF_LOGF_IFERR(error_0, "");

        next_free_slot();
        sel4_page = dest_index;
#endif

        /* XXX: Write-only mappings are silently downgraded by the kernel to
         * kernel-only. This is clearly not what the user intended if they
         * passed us a write-only mapping. Help them out by upgrading it here.
         */
        if (seL4_CapRights_get_capAllowWrite(rights)) {
            rights = seL4_CapRights_set_capAllowRead(rights, true);
        }

#ifdef CONFIG_ARCH_ARM
        if (!seL4_CapRights_get_capAllowGrant(rights)) {
            vm_attribs |= seL4_ARM_ExecuteNever;
        }
#endif

        /* Store the cap used for mapping the page in the CDL_Cap in the
         * corresponding page table/directory slot for later use. */
        page_cap->mapped_frame_cap = sel4_page;

        // FIXME: Add support for super-pages.
        int error = seL4_ARCH_Page_Map(sel4_page, sel4_pd, vaddr, rights, vm_attribs);
        if (error) {
            /* Try and retrieve some useful information to help the user
             * diagnose the error.
             */
            ZF_LOGE("Failed to map frame ");
            seL4_ARCH_Page_GetAddress_t addr UNUSED = seL4_ARCH_Page_GetAddress(sel4_page);
            if (addr.error) {
                ZF_LOGE("<unknown physical address (error = %d)>", addr.error);
            } else {
                ZF_LOGE("%p", (void *)addr.paddr);
            }
            ZF_LOGE(" -> %p (error = %d)", (void *)vaddr, error);
            ZF_LOGF_IFERR(error, "");
        }
#ifdef CONFIG_ARCH_ARM
        /* When seL4 creates a new frame object it zeroes the associated memory
         * through a cached kernel mapping. If we are configuring a cached
         * mapping for the target, standard coherence protocols ensure
         * everything works as expected. However, if we are configuring an
         * uncached mapping for the target, the dirty zero data cached from the
         * kernel's mapping is likely flushed to memory at some time in the
         * future causing an unpleasant surprise for the target whose own
         * uncached writes are mysteriously overwritten. To prevent this, we
         * unify the mapping here, flushing the cached data from the kernel's
         * mapping.
         */
        seL4_Word size_bits = spec->objects[page].size_bits;
        assert(size_bits <= sizeof(uintptr_t) * CHAR_BIT - 1 && "illegal object size");

        seL4_ARCH_Page_GetAddress_t addr = seL4_ARCH_Page_GetAddress(sel4_page);
        if (addr.paddr >= memory_region[0].start && addr.paddr <= memory_region[0].end) {
            if (!(vm_attribs & seL4_ARM_PageCacheable) && CDL_Obj_Paddr(&spec->objects[page]) == 0) {
                error = seL4_ARM_Page_CleanInvalidate_Data(sel4_page, 0, BIT(size_bits));
                ZF_LOGF_IFERR(error, "");
            }

            if (seL4_CapRights_get_capAllowGrant(rights)) {
                error = seL4_ARM_Page_Unify_Instruction(sel4_page, 0, BIT(size_bits));
                ZF_LOGF_IFERR(error, "");
            }
        }
#endif
    } else {
        ZF_LOGF("attempt to map something that is not a frame or PT");
    }
}

#if defined(CONFIG_ARCH_X86_64) || defined(CONFIG_ARCH_AARCH64) || defined(CONFIG_ARCH_RISCV)

static void init_level_3(CDL_Model *spec, CDL_ObjID level_0_obj, uintptr_t level_3_base, CDL_ObjID level_3_obj)
{
    CDL_Object *obj = get_spec_object(spec, level_3_obj);
    for (unsigned long slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = level_3_base + (obj_slot << (seL4_PageBits));
        CDL_Cap *frame_cap = CDL_CapSlot_Cap(slot);
        seL4_CapRights_t frame_rights = CDL_seL4_Cap_Rights(frame_cap);
        map_page(spec, frame_cap, level_0_obj, frame_rights, base);
    }
}

#if (CDL_PT_NUM_LEVELS >= 2)
static void init_level_2(CDL_Model *spec, CDL_ObjID level_0_obj, uintptr_t level_2_base, CDL_ObjID level_2_obj)
{
    CDL_Object *obj = get_spec_object(spec, level_2_obj);
    for (unsigned long slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = level_2_base + (obj_slot << (CDL_PT_LEVEL_3_IndexBits + seL4_PageBits));
        CDL_Cap *level_3_cap = CDL_CapSlot_Cap(slot);
        CDL_ObjID level_3_obj = CDL_Cap_ObjID(level_3_cap);
        if (CDL_Cap_Type(level_3_cap) == CDL_FrameCap) {
            seL4_CapRights_t frame_rights = CDL_seL4_Cap_Rights(level_3_cap);
            map_page(spec, level_3_cap, level_0_obj, frame_rights, base);
        } else {
            seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(level_3_cap);
            CDL_PT_LEVEL_3_MAP(orig_caps(level_3_obj), orig_caps(level_0_obj), base, vm_attribs);
            init_level_3(spec, level_0_obj, base, level_3_obj);
        }
    }
}
#endif

#if (CDL_PT_NUM_LEVELS >= 3)
static void init_level_1(CDL_Model *spec, CDL_ObjID level_0_obj, uintptr_t level_1_base, CDL_ObjID level_1_obj)
{
    CDL_Object *obj = get_spec_object(spec, level_1_obj);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = level_1_base + (obj_slot << (CDL_PT_LEVEL_2_IndexBits + CDL_PT_LEVEL_3_IndexBits + seL4_PageBits));
        CDL_Cap *level_2_cap = CDL_CapSlot_Cap(slot);
        CDL_ObjID level_2_obj = CDL_Cap_ObjID(level_2_cap);
        if (CDL_Cap_Type(level_2_cap) == CDL_FrameCap) {
            seL4_CapRights_t frame_rights = CDL_seL4_Cap_Rights(level_2_cap);
            map_page(spec, level_2_cap, level_0_obj, frame_rights, base);
        } else {
            seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(level_2_cap);
            CDL_PT_LEVEL_2_MAP(orig_caps(level_2_obj), orig_caps(level_0_obj), base, vm_attribs);
            init_level_2(spec, level_0_obj, base, level_2_obj);
        }
    }
}
#endif

#if (CDL_PT_NUM_LEVELS >= 4)
static void init_level_0(CDL_Model *spec, CDL_ObjID level_0_obj, uintptr_t level_0_base, CDL_ObjID level_0_obj_unused)
{
    CDL_Object *obj = get_spec_object(spec, level_0_obj);
    for (unsigned long slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = (level_0_base + obj_slot) << (CDL_PT_LEVEL_1_IndexBits + CDL_PT_LEVEL_2_IndexBits +
                                                       CDL_PT_LEVEL_3_IndexBits + seL4_PageBits);
        CDL_Cap *level_1_cap = CDL_CapSlot_Cap(slot);
        CDL_ObjID level_1_obj = CDL_Cap_ObjID(level_1_cap);
        seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(level_1_cap);
#ifdef CDL_PT_LEVEL_1_MAP
        CDL_PT_LEVEL_1_MAP(orig_caps(level_1_obj), orig_caps(level_0_obj), base, vm_attribs);
        init_level_1(spec, level_0_obj, base, level_1_obj);
#else
        ZF_LOGF("CDL_PT_LEVEL_1_MAP is not defined");
#endif
    }
}
#endif

#else

static void map_page_directory_slot(CDL_Model *spec UNUSED, CDL_ObjID pd_id, CDL_CapSlot *pd_slot)
{
    ZF_LOGD("  Mapping slot %d in %s", pd_slot->slot, CDL_Obj_Name(&spec->objects[pd_id]));
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pd_slot);

    seL4_Word page_vaddr = CDL_CapSlot_Slot(pd_slot) << (seL4_PageTableIndexBits + seL4_PageBits);
    seL4_CapRights_t page_rights = CDL_seL4_Cap_Rights(page_cap);

    map_page(spec, page_cap, pd_id, page_rights, page_vaddr);
}

static void map_page_directory(CDL_Model *spec, CDL_ObjID pd_id)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd_id);

    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++) {
        map_page_directory_slot(spec, pd_id, CDL_Obj_GetSlot(cdl_pd, slot_index));
    }
}

static void map_page_table_slot(CDL_Model *spec UNUSED, CDL_ObjID pd, CDL_ObjID pt UNUSED,
                                seL4_Word pt_vaddr, CDL_CapSlot *pt_slot)
{
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pt_slot);

    seL4_Word page_vaddr = pt_vaddr + (CDL_CapSlot_Slot(pt_slot) << seL4_PageBits);
    seL4_CapRights_t page_rights = CDL_seL4_Cap_Rights(page_cap);

    ZF_LOGD("  Mapping %s into %s[%d] with rights={G: %d, R: %d, W: %d}, vaddr=0x%" PRIxPTR "",
            CDL_Obj_Name(&spec->objects[pt]), CDL_Obj_Name(&spec->objects[pd]), pt_slot->slot,
            seL4_CapRights_get_capAllowGrant(page_rights),
            seL4_CapRights_get_capAllowRead(page_rights),
            seL4_CapRights_get_capAllowWrite(page_rights),
            (uintptr_t)pt_vaddr);

    map_page(spec, page_cap, pd, page_rights, page_vaddr);
}

static void map_page_table_slots(CDL_Model *spec, CDL_ObjID pd, CDL_CapSlot *pd_slot)
{
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pd_slot);
    CDL_ObjID page = CDL_Cap_ObjID(page_cap);

    seL4_Word page_vaddr = CDL_CapSlot_Slot(pd_slot) << (seL4_PageTableIndexBits + seL4_PageBits);

    if (CDL_Cap_Type(page_cap) == CDL_PTCap) {
        CDL_Object *obj = get_spec_object(spec, page);
        for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
            map_page_table_slot(spec, pd, page, page_vaddr, CDL_Obj_GetSlot(obj, slot_index));
        }
    }
}

static void map_page_directory_page_tables(CDL_Model *spec, CDL_ObjID pd)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++) {
        map_page_table_slots(spec, pd, CDL_Obj_GetSlot(cdl_pd, slot_index));
    }
}
#endif

static void init_vspace(CDL_Model *spec)
{
    ZF_LOGD("Initialising VSpaces...");

#if defined(CONFIG_ARCH_X86_64) || defined(CONFIG_ARCH_AARCH64) || defined(CONFIG_ARCH_RISCV)
    /* Have no understanding of the logic of model of whatever the hell the
       other code in this function is doing as it is pure gibberish. For
       x86_64 and aarch64 we will just do the obvious recursive initialization */
    ZF_LOGD("================================");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TOP_LEVEL_PD) {
            ZF_LOGD(" Initialising top level %s...", CDL_Obj_Name(&spec->objects[obj_id]));
#if (CDL_PT_NUM_LEVELS == 4)
            init_level_0(spec, obj_id, 0, obj_id);
#elif (CDL_PT_NUM_LEVELS == 3)
            init_level_1(spec, obj_id, 0, obj_id);
#elif (CDL_PT_NUM_LEVELS == 2)
            init_level_2(spec, obj_id, 0, obj_id);
#else
            ZF_LOGF("Unsupported CDL_PT_NUM_LEVELS value: \"%d\"", CDL_PT_NUM_LEVELS);
#endif
        }
    }
#else
    ZF_LOGD("================================");
    ZF_LOGD("Initialising page directories...");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PD) {
            ZF_LOGD(" Initialising page directory %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            map_page_directory(spec, obj_id);
        }
    }

    ZF_LOGD("===========================");
    ZF_LOGD("Initialising page tables...");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PD) {
            ZF_LOGD(" Initialising page tables in %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            map_page_directory_page_tables(spec, obj_id);
        }
    }
#endif
}

static bool ep_related_cap(CDL_CapType cap)
{
    switch (cap) {
    case CDL_EPCap:
    case CDL_NotificationCap:
    case CDL_ReplyCap:
        return true;
    default:
        return false;
    }
}

/* Initialise capability spaces */
static void init_cnode_slot(CDL_Model *spec, init_cnode_mode mode, CDL_ObjID cnode_id, CDL_CapSlot *cnode_slot)
{
    CDL_Cap *target_cap = CDL_CapSlot_Cap(cnode_slot);
    CDL_ObjID target_cap_obj = CDL_Cap_ObjID(target_cap);
    CDL_IRQ target_cap_irq = CDL_Cap_IRQ(target_cap);

    CDL_CapType target_cap_type = CDL_Cap_Type(target_cap);
    seL4_CapRights_t target_cap_rights = CDL_seL4_Cap_Rights(target_cap);

    // For endpoint this is the badge, for cnodes, this is the (encoded) guard.
    seL4_Word target_cap_data = get_capData(CDL_Cap_Data(target_cap));

    /* To support moving original caps, we need a spec with a CDT (most don't).
     * This shoud probably become a separate configuration option for when to
     * use the CDT, and when to just copy. For now, let's just copy.
     */
    bool move_cap = false; //FIXME
    bool is_ep_cap = ep_related_cap(target_cap_type);
    bool is_irq_handler_cap = (target_cap_type == CDL_IRQHandlerCap);
    bool is_frame_cap = (target_cap_type == CDL_FrameCap);

    CDL_Object *dest_obj = get_spec_object(spec, cnode_id);
    uint8_t dest_size = CDL_Obj_SizeBits(dest_obj);

    // Use a copy of the cap to reference the destination, in case the original has already been moved.
    seL4_CPtr dest_root = dup_caps(cnode_id);
    int dest_index = CDL_CapSlot_Slot(cnode_slot);
    uint8_t dest_depth = dest_size;

    // Use an original cap to reference the object to copy.
    seL4_CPtr src_root = seL4_CapInitThreadCNode;
    int src_index;
    switch (target_cap_type) {
#ifdef CONFIG_ARCH_X86
    case CDL_IOSpaceCap:
        src_index = seL4_CapIOSpace;
        break;
#endif
#if defined(CONFIG_ARCH_ARM)
    case CDL_ARMIOSpaceCap:
        src_index = first_arm_iospace + target_cap_data;
        target_cap_data = seL4_NilData;
        break;
#endif
    case CDL_IRQHandlerCap:
        src_index = irq_caps(target_cap_irq);
        break;
    case CDL_IRQControlCap:
        /* there's only one */
        src_index = seL4_CapIRQControl;
        move_cap = true;
        is_irq_handler_cap = true;
        break;
    case CDL_SchedControlCap:
        src_index = sched_ctrl_caps(CDL_Cap_ObjID(target_cap));
        break;
    case CDL_DomainCap:
        /* there's only one */
        src_index = seL4_CapDomain;
        move_cap = false;
        break;
    case CDL_ASIDControlCap:
        /* there's only one */
        src_index = seL4_CapASIDControl;
        move_cap = false;
        break;
    default:
        src_index = orig_caps(target_cap_obj);
        break;
    }

    uint8_t src_depth = CONFIG_WORD_SIZE;

    if (mode == MOVE && move_cap) {
        if (is_ep_cap || is_irq_handler_cap) {
            ZF_LOGD("moving...");
            int error = seL4_CNode_Move(dest_root, dest_index, dest_depth,
                                        src_root, src_index, src_depth);
            ZF_LOGF_IFERR(error, "");
        } else {
            ZF_LOGD("mutating (with badge/guard %p)...", (void *)target_cap_data);
            int error = seL4_CNode_Mutate(dest_root, dest_index, dest_depth,
                                          src_root, src_index, src_depth, target_cap_data);
            ZF_LOGF_IFERR(error, "");
        }
    } else if (mode == COPY && !move_cap) {
        if (is_frame_cap && target_cap->mapping_container_id != INVALID_OBJ_ID) {
            ZF_LOGD("moving mapped...");
            /* The spec requires the frame cap in the current slot be the same one
             * used to perform the mapping of the frame in some container (either
             * a page table or page directory). */
            CDL_ObjID container_id = target_cap->mapping_container_id;
            seL4_Word slot_index = target_cap->mapping_slot;

            /* Look up the container object which contains the mapping. */
            CDL_Object *container = get_spec_object(spec, container_id);
            assert(container);
            /* When the frame was mapped in, a copy of the cap was first created,
             * and the copy used for the mapping. This copy is the cap that must
             * be moved into the current slot. */
            CDL_Cap *frame_cap = get_cap_at(container, slot_index);
            assert(frame_cap);
            assert(frame_cap->type == CDL_FrameCap);
            seL4_CPtr mapped_frame_cap = frame_cap->mapped_frame_cap;

            /* Move the cap to the frame used for the mapping into the destination slot. */
            int error = seL4_CNode_Move(dest_root, dest_index, dest_depth,
                                        src_root, mapped_frame_cap, src_depth);
            ZF_LOGF_IFERR(error, "");
        } else {
            ZF_LOGD("minting (with badge/guard %p)...", (void *)target_cap_data);
            int error = seL4_CNode_Mint(dest_root, dest_index, dest_depth,
                                        src_root, src_index, src_depth, target_cap_rights, target_cap_data);
            ZF_LOGF_IFERR(error, "");
        }
    } else {
        ZF_LOGV("skipping");
    }
}

static void init_cnode(CDL_Model *spec, init_cnode_mode mode, CDL_ObjID cnode)
{
    CDL_Object *cdl_cnode = get_spec_object(spec, cnode);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_cnode); slot_index++) {
        if (CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.type == CDL_IRQHandlerCap) {
            CDL_IRQ UNUSED irq = CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.irq;
            ZF_LOGD("  Populating slot %d with cap to IRQ %d, name %s...",
                    CDL_Obj_GetSlot(cdl_cnode, slot_index)->slot, irq,
                    CDL_Obj_Name(&spec->objects[spec->irqs[irq]]));
        } else {
            ZF_LOGD("  Populating slot %d with cap to %s...",
                    CDL_Obj_GetSlot(cdl_cnode, slot_index)->slot,
                    CDL_Obj_Name(&spec->objects[CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.obj_id]));
        }
        init_cnode_slot(spec, mode, cnode, CDL_Obj_GetSlot(cdl_cnode, slot_index));
    }
}

static void init_cspace(CDL_Model *spec)
{
    ZF_LOGD("Copying Caps...");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode) {
            ZF_LOGD(" Copying into %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            init_cnode(spec, COPY, obj_id);
        }
    }

    ZF_LOGD("Moving Caps...");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode) {
            ZF_LOGD(" Moving into %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            init_cnode(spec, MOVE, obj_id);
        }
    }
}

static void start_threads(CDL_Model *spec)
{
    ZF_LOGD("Starting threads...");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB && spec->objects[obj_id].tcb_extra.resume) {
            ZF_LOGD(" Starting %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            seL4_CPtr tcb = orig_caps(obj_id);
            int error = seL4_TCB_Resume(tcb);
            ZF_LOGF_IFERR(error, "");
        }
    }
}


static void init_copy_addr(seL4_BootInfo *bi)
{
    /* We need a page sized and aligned region at which to map the
     * destination frame during loading. We know we have free memory
     * after the end of our binary image + any additional frames
     * the kernel has mapped. The kernel maps 1 frame for IPC buffer
     * 1 frame for bootinfo and on some platforms additional extended
     * bootinfo frames. So we skip these frames and then round up to
     * the next 16mb alignment where we can map in a pagetable.
     */
    uintptr_t bi_start = (uintptr_t)bi;
    copy_addr = ROUND_UP(bi_start + PAGE_SIZE_4K + bi->extraLen, 0x1000000);
}

static void cache_extended_bootinfo_headers(seL4_BootInfo *bi)
{
    uintptr_t cur = (uintptr_t)bi + PAGE_SIZE_4K;
    uintptr_t end = cur + bi->extraLen;

    while (cur < end) {
        seL4_BootInfoHeader *header = (seL4_BootInfoHeader *)cur;
        extended_bootinfo_table[header->id] = header;
        cur += header->len;
    }
}

static void fill_frame_with_bootinfo(uintptr_t base, CDL_FrameFill_Element_t frame_fill)
{
    CDL_FrameFill_BootInfoEnum_t bi_type = frame_fill.bi_type.type;
    switch (bi_type) {
    case CDL_FrameFill_BootInfo_X86_VBE:
    case CDL_FrameFill_BootInfo_X86_TSC_Freq:
    case CDL_FrameFill_BootInfo_FDT:
        break;
    default:
        ZF_LOGF("Unable to parse extra information for \"bootinfo\", given \"%d\"",
                bi_type);
    }

    uintptr_t dest = base + frame_fill.dest_offset;
    size_t max_len = frame_fill.dest_len;
    size_t block_offset = frame_fill.bi_type.src_offset;
    seL4_BootInfoHeader *header = extended_bootinfo_table[bi_type];

    /* Check if the bootinfo has been found */
    if (header) {
        /* Don't copy past the bootinfo */
        size_t copy_len = header->len < block_offset ? 0 : header->len - block_offset;
        /* Don't copy more than what the frame can hold */
        copy_len = MIN(copy_len, max_len);
        void *copy_start = (void *) header + block_offset;
        memcpy((void *) dest, copy_start, copy_len);
    } else {
        /* Bootinfo hasn't been found.
         * If we're at the start of a block, copy an empty header across, otherwise skip the copy */
        if (block_offset == 0) {
            seL4_BootInfoHeader empty = (seL4_BootInfoHeader) {
                .id = -1, .len = -1
            };
            size_t copy_len = MIN(sizeof(empty), max_len);
            memcpy((void *)base, &empty, copy_len);
        }
    }
}

static void fill_frame_with_filedata(uintptr_t base, CDL_FrameFill_Element_t frame_fill)
{
    unsigned long file_size;
    unsigned long cpio_size = _capdl_archive_end - _capdl_archive;
    const void *file = cpio_get_file(_capdl_archive, cpio_size, frame_fill.file_data_type.filename, &file_size);
    memcpy((void *)base + frame_fill.dest_offset, file + frame_fill.file_data_type.file_offset, frame_fill.dest_len);
}

static void init_frame(CDL_Model *spec, CDL_ObjID obj_id, CDL_FrameFill_Element_t frame_fill)
{
    seL4_CPtr cap = orig_caps(obj_id);

    /* get the cap to the original object */
    /* try a large mapping */
    uintptr_t base = (uintptr_t)copy_addr;
    int error = seL4_ARCH_Page_Map(cap, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                                   seL4_ReadWrite, seL4_ARCH_Default_VMAttributes);
    if (error == seL4_FailedLookup) {
        /* try a small mapping */
        base = (uintptr_t)copy_addr_with_pt;
        error = seL4_ARCH_Page_Map(cap, seL4_CapInitThreadPD, (seL4_Word)copy_addr_with_pt,
                                   seL4_ReadWrite, seL4_ARCH_Default_VMAttributes);
    }
    ZF_LOGF_IFERR(error, "");

    ssize_t max = BIT(spec->objects[obj_id].size_bits) - frame_fill.dest_offset;
    ZF_LOGF_IF(frame_fill.dest_len > max, "Bad spec, fill frame with len larger than frame size");

    /* Check for which type */
    switch (frame_fill.type) {
    case CDL_FrameFill_BootInfo:
        fill_frame_with_bootinfo(base, frame_fill);
        break;
    case CDL_FrameFill_FileData:
        fill_frame_with_filedata(base, frame_fill);
        break;
    default:
        ZF_LOGF("Unsupported frame fill type %u", frame_fill.type);
    }

    /* Unmap the frame */
    error = seL4_ARCH_Page_Unmap(cap);
    ZF_LOGF_IFERR(error, "");
}

static void init_frames(CDL_Model *spec)
{
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_Frame) {
            for (int i = 0; i < CONFIG_CAPDL_LOADER_FILLS_PER_FRAME
                 && spec->objects[obj_id].frame_extra.fill[i].type != CDL_FrameFill_None; i++) {
                CDL_FrameFill_Element_t frame_fill = spec->objects[obj_id].frame_extra.fill[i];
                init_frame(spec, obj_id, frame_fill);
            }
        }
    }
}

static void init_scs(CDL_Model *spec)
{
    ZF_LOGD(" Initialising SCs");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_SchedContext) {
            ZF_LOGD(" Initialising %s...", CDL_Obj_Name(&spec->objects[obj_id]));
            /* all scs get configured on core 0, any scs that should be bound to a tcb will
               be reconfigured for the correct core in init_tcbs */
            init_sc(spec, obj_id, 0);
        }
    }
}

#ifdef CONFIG_ARCH_RISCV
/**
 * RISC-V uses a PageTable object as all table objects in the address structure.
 * in several places this loader assumes that the root VSpace object is a unique
 * object type and can be iterated over in the spec for performing operations on
 * a vspace_root. This function updates the CDL object type of all PageTables that
 * exist in a TCB VSpace slot to CDL_PT_ROOT_ALIAS which allows the rest of the
 * loader to treat the roots as unique objects.
 */
static void mark_vspace_roots(CDL_Model *spec)
{
    ZF_LOGD("Marking top level PageTables as CDL_PT_ROOT_ALIAS...");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        CDL_ObjectType type = CDL_TCB;
        if (spec->objects[obj_id].type == type) {
            CDL_ObjID root = CDL_Cap_ObjID(get_cap_at(get_spec_object(spec, obj_id), CDL_TCB_VTable_Slot));
            ZF_LOGD(" Updating vspace_root: %d", root);
            spec->objects[root].type = CDL_PT_ROOT_ALIAS;
        }
    }
}
#endif


static void init_system(CDL_Model *spec)
{
    seL4_BootInfo *bootinfo = platsupport_get_bootinfo();
    simple_t simple;

    cache_extended_bootinfo_headers(bootinfo);
    init_copy_addr(bootinfo);

    simple_default_init_bootinfo(&simple, bootinfo);

    init_copy_frame(bootinfo);

    parse_bootinfo(bootinfo, spec);

    create_objects(spec, bootinfo);
#ifdef CONFIG_ARCH_RISCV
    /*
     * This needs to be called after create_objects as it modifies parts of the
     * spec that create_objects uses, but are _hopefully_ safe to change after.
     */
    mark_vspace_roots(spec);
#endif
    create_irq_caps(spec);
    if (config_set(CONFIG_KERNEL_MCS)) {
        create_sched_ctrl_caps(bootinfo);
    }
    duplicate_caps(spec);

    init_irqs(spec);
    init_pd_asids(spec);
    init_frames(spec);
    init_vspace(spec);
    init_scs(spec);
    init_tcbs(spec);
    init_cspace(spec);
    start_threads(spec);

    ZF_LOGD("%d of %d CSlots used (%.2LF%%)", get_free_slot(),
            BIT(CONFIG_ROOT_CNODE_SIZE_BITS),
            ((long double)get_free_slot() / BIT(CONFIG_ROOT_CNODE_SIZE_BITS))
            * 100);

}

#ifdef CONFIG_DEBUG_BUILD
/* We need to give malloc enough memory for musllibc to allocate memory
 * for stdin, stdout, and stderr. The heap pool base address and size is
 * expected to be page aligned.
 */
extern char *morecore_area;
extern size_t morecore_size;
static char ALIGN(PAGE_SIZE_4K) debug_libc_morecore_area[PAGE_SIZE_4K];

static void CONSTRUCTOR(MUSLCSYS_WITH_VSYSCALL_PRIORITY) init_bootinfo(void)
{
    /* Init memory area for musl. */
    morecore_area = debug_libc_morecore_area;
    morecore_size = sizeof(debug_libc_morecore_area);

    /* Allow us to print via seL4_Debug_PutChar. */
    platsupport_serial_setup_bootinfo_failsafe();
}
#endif

int main(void)
{
    ZF_LOGI("Starting CapDL Loader...");
    init_system(&capdl_spec);
    ZF_LOGI(A_RESET A_FG_G "CapDL Loader done, suspending..." A_RESET "");
    seL4_TCB_Suspend(seL4_CapInitThreadTCB);
}
