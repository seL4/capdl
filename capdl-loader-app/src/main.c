/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#include <autoconf.h>

#include <assert.h>
#include <inttypes.h>
#include <limits.h>

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <elf/elf.h>
#include <sel4platsupport/platsupport.h>
#include <cpio/cpio.h>
#include <simple-default/simple-default.h>

#include <vka/kobject_t.h>
#include <utils/util.h>
#include <sel4/sel4.h>
#include <sel4utils/sel4_zf_logif.h>
#include "capdl.h"

#include "capdl_spec.h"

#define PML4_SLOT(vaddr) ((vaddr >> (seL4_PDPTIndexBits + seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PML4IndexBits))
#define PDPT_SLOT(vaddr) ((vaddr >> (seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PDPTIndexBits))
#define PD_SLOT(vaddr)   ((vaddr >> (seL4_PageTableIndexBits + seL4_PageBits)) & MASK(seL4_PageDirIndexBits))
#define PT_SLOT(vaddr)   ((vaddr >> seL4_PageBits) & MASK(seL4_PageTableIndexBits))

#define CAPDL_SHARED_FRAMES

#define STACK_ALIGNMENT_BYTES 16

static seL4_CPtr capdl_to_sel4_orig[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_copy[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_irq[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sched_ctrl[CONFIG_MAX_NUM_NODES];
// List of untyped cptrs, sorted from largest to smallest.
static seL4_CPtr untyped_cptrs[CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS];

static seL4_CPtr free_slot_start, free_slot_end;

static seL4_CPtr first_arm_iospace;

// Hack for seL4_TCB_WriteRegisters because we can't take the address of local variables.
static seL4_UserContext global_user_context;

extern char _capdl_archive[];

/* This symbol is provided by the GNU linker and points at the start/end of our
 * ELF image.
 */
extern char __executable_start[];
extern char _end[];

/* We need a page sized and aligned region at which to map the
 * destination frame during loading. We know we have free memory
 * after the end of our binary image + any additional frames
 * the kernel has mapped. The kernel maps 1 frame for IPC buffer
 * 1 frame for bootinfo, and on some platforms an additional 1
 * frame of bootinfo. So we skip three frames and then round up
 * to the next 16mb alignment where we can map in a pagetable.
 */
#define copy_addr ( ROUND_UP(((uintptr_t)_end) + (PAGE_SIZE_4K * 3), 0x1000000))

/* In the case where we just want a 4K page and we cannot allocate
 * a page table ourselves, we use this pre allocated region that
 * is guaranteed to have a pagetable */
static char copy_addr_with_pt[PAGE_SIZE_4K] __attribute__((aligned(PAGE_SIZE_4K)));

/* helper functions ---------------------------------------------------------------------------- */

static seL4_CPtr
get_free_slot(void)
{
    return free_slot_start;
}

static void
next_free_slot(void)
{
    free_slot_start++;
    ZF_LOGF_IF(free_slot_start >= free_slot_end, "Ran out of free slots!");
}

typedef enum {MOVE, COPY} init_cnode_mode;
typedef enum {ORIG, DUP, IRQ, SCHED_CTRL} seL4_cap_type;

static seL4_CPtr
orig_caps(CDL_ObjID object_id)
{
    assert(object_id < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_orig[object_id];
}

static seL4_CPtr
dup_caps(CDL_ObjID object_id)
{
    assert(object_id < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_copy[object_id];
}

static seL4_CPtr
irq_caps(CDL_IRQ irq)
{
    assert(irq < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_irq[irq];
}

static seL4_CPtr
sched_ctrl_caps(CDL_Core id)
{
    assert(id < CONFIG_MAX_NUM_NODES);
    return capdl_to_sched_ctrl[id];
}

static void
add_sel4_cap(CDL_ObjID object_id, seL4_cap_type type, seL4_CPtr slot)
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

static seL4_Word
get_capData(CDL_CapData d)
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

static CDL_Cap *
get_cap_at(CDL_Object *obj, unsigned int slot)
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

static CDL_Cap *get_cdl_frame_pt(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
#ifdef CONFIG_ARCH_X86_64
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

static seL4_CPtr
get_frame_pt(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
    CDL_Cap *pt_cap = get_cdl_frame_pt(pd, vaddr, spec);

    /* Check if the PT cap is actually a large frame cap. */
    if (pt_cap->type == CDL_FrameCap) {
        return 0;
    }
    assert(orig_caps(CDL_Cap_ObjID(pt_cap)) != 0);

    return orig_caps(CDL_Cap_ObjID(pt_cap));
}

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
static seL4_CPtr
get_frame_cap(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
    return orig_caps(CDL_Cap_ObjID(get_cdl_frame_cap(pd, vaddr, spec)));
}

static seL4_CPtr
get_frame_size(CDL_ObjID pd, uintptr_t vaddr, CDL_Model *spec)
{
    return BIT(CDL_Obj_SizeBits(&spec->objects[CDL_Cap_ObjID(get_cdl_frame_cap(pd, vaddr, spec))]));
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
#ifdef CONFIG_ARCH_X6_64
    /* guess that there is one PDPT and PML4 */
    copy_addr_pt += 2;
#endif

    int error;

    for (int i = 0; i < sizeof(copy_addr_with_pt) / PAGE_SIZE_4K; i++) {
#ifdef CONFIG_ARCH_ARM
        error = seL4_ARM_Page_Unify_Instruction(copy_addr_frame + i, 0, PAGE_SIZE_4K);
        ZF_LOGF_IFERR(error, "");
#endif
        error = seL4_ARCH_Page_Unmap(copy_addr_frame + i);
        ZF_LOGF_IFERR(error, "");

        if ((i + 1) % BIT(seL4_PageTableIndexBits) == 0) {
            error = seL4_ARCH_PageTable_Unmap(copy_addr_pt + i / BIT(seL4_PageTableIndexBits));
            ZF_LOGF_IFERR(error, "");
        }
    }
}

static void
elf_load_frames(const char *elf_name, CDL_ObjID pd, CDL_Model *spec,
                seL4_BootInfo *bootinfo)
{
    unsigned long elf_size;
    void *elf_file = cpio_get_file(_capdl_archive, elf_name, &elf_size);

    if (elf_file == NULL) {
        ZF_LOGF("ELF file %s not found", elf_name);
    }

    if (elf_checkFile(elf_file) != 0) {
        ZF_LOGF("Unable to read elf file %s at %p", elf_name, elf_file);
    }

    ZF_LOGD("   ELF loading %s (from %p)... \n", elf_name, elf_file);

    for (int i = 0; i < elf_getNumProgramHeaders(elf_file); i++) {
        ZF_LOGD("    to %p... ", (void*)(uintptr_t)elf_getProgramHeaderVaddr(elf_file, i));

        size_t f_len = elf_getProgramHeaderFileSize(elf_file, i);
        uintptr_t dest = elf_getProgramHeaderVaddr(elf_file, i);
        uintptr_t src = (uintptr_t) elf_file + elf_getProgramHeaderOffset(elf_file, i);

        //Skip non loadable headers
        if (elf_getProgramHeaderType(elf_file, i) != PT_LOAD) {
            ZF_LOGD("Skipping non loadable header\n");
            continue;
        }
        uintptr_t vaddr = dest;

        while (vaddr < dest + f_len) {
            ZF_LOGD(".");

            /* map frame into the loader's address space so we can write to it */
            seL4_CPtr sel4_page = get_frame_cap(pd, vaddr, spec);
            seL4_CPtr sel4_page_pt = get_frame_pt(pd, vaddr, spec);
            size_t sel4_page_size = get_frame_size(pd, vaddr, spec);

            seL4_ARCH_VMAttributes attribs = seL4_ARCH_Default_VMAttributes;
#ifdef CONFIG_ARCH_ARM
            attribs |= seL4_ARM_ExecuteNever;
#endif

            int error = seL4_ARCH_Page_Map(sel4_page, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                                           seL4_ReadWrite, attribs);
            if (error == seL4_FailedLookup) {
                error = seL4_ARCH_PageTable_Map(sel4_page_pt, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                                                seL4_ARCH_Default_VMAttributes);
                ZF_LOGF_IFERR(error, "");
                error = seL4_ARCH_Page_Map(sel4_page, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                                           seL4_ReadWrite, attribs);
            }
            if (error) {
                /* Try and retrieve some useful information to help the user
                 * diagnose the error.
                 */
                ZF_LOGD("Failed to map frame ");
                seL4_ARCH_Page_GetAddress_t addr UNUSED = seL4_ARCH_Page_GetAddress(sel4_page);
                if (addr.error) {
                    ZF_LOGD("<unknown physical address (error = %d)>", addr.error);
                } else {
                    ZF_LOGD("%p", (void*)addr.paddr);
                }
                ZF_LOGD(" -> %p (error = %d)\n", (void*)copy_addr, error);
                ZF_LOGF_IFERR(error, "");
            }

            /* copy until end of section or end of page */
            size_t len = dest + f_len - vaddr;
            if (len > sel4_page_size - (vaddr % sel4_page_size)) {
                len = sel4_page_size - (vaddr % sel4_page_size);
            }
            memcpy((void *) (copy_addr + vaddr % sel4_page_size), (void *) (src + vaddr - dest), len);

#ifdef CONFIG_ARCH_ARM
            error = seL4_ARM_Page_Unify_Instruction(sel4_page, 0, sel4_page_size);
            ZF_LOGF_IFERR(error, "");
#endif
            error = seL4_ARCH_Page_Unmap(sel4_page);
            ZF_LOGF_IFERR(error, "");

            if (sel4_page_pt != 0) {
                error = seL4_ARCH_PageTable_Unmap(sel4_page_pt);
                ZF_LOGF_IFERR(error, "");
            }

            vaddr += len;
        }

        /* Overwrite the section type so that next time this section is
         * encountered it will be skipped as it is not considered loadable. A
         * bit of a hack, but fine for now.
         */
        ZF_LOGD(" Marking header as loaded\n");
        if (((struct Elf32_Header*)elf_file)->e_ident[EI_CLASS] == ELFCLASS32) {
            elf32_getProgramHeaderTable(elf_file)[i].p_type = PT_NULL;
        } else if (((struct Elf64_Header*)elf_file)->e_ident[EI_CLASS] == ELFCLASS64) {
            elf64_getProgramHeaderTable(elf_file)[i].p_type = PT_NULL;
        }
    }
}

/* Sort the untyped objects from largest to smallest.
 * This ensures that fragmentation is eliminated if the objects
 * themselves are also sorted, largest to smallest.
 *
 * Sorting done using counting sort.
 */
static void
sort_untypeds(seL4_BootInfo *bootinfo)
{
    seL4_CPtr untyped_start = bootinfo->untyped.start;
    seL4_CPtr untyped_end = bootinfo->untyped.end;

    ZF_LOGD("Sorting untypeds...\n");

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

    // Store untypeds in untyped_cptrs array.
    for (seL4_Word untyped_index = 0; untyped_index != untyped_end - untyped_start; untyped_index++) {
        if (bootinfo->untypedList[untyped_index].isDevice) {
            ZF_LOGD("Untyped %3d (cptr=%p) (addr=%p) is of size %2d. Skipping as it is device\n",
                    untyped_index, (void*)(untyped_start + untyped_index),
                    (void*)bootinfo->untypedList[untyped_index].paddr,
                    bootinfo->untypedList[untyped_index].sizeBits);
        } else {
            ZF_LOGD("Untyped %3d (cptr=%p) (addr=%p) is of size %2d. Placing in slot %d...\n",
                    untyped_index, (void*)(untyped_start + untyped_index),
                    (void*)bootinfo->untypedList[untyped_index].paddr,
                    bootinfo->untypedList[untyped_index].sizeBits,
                    count[bootinfo->untypedList[untyped_index].sizeBits]);

            untyped_cptrs[count[bootinfo->untypedList[untyped_index].sizeBits]] = untyped_start +  untyped_index;
            count[bootinfo->untypedList[untyped_index].sizeBits] += 1;
        }
    }

}

static void
parse_bootinfo(seL4_BootInfo *bootinfo)
{
    ZF_LOGD("Parsing bootinfo...\n");

    free_slot_start = bootinfo->empty.start;
    free_slot_end = bootinfo->empty.end;

    /* When using libsel4platsupport for printing support, we end up using some
     * of our free slots during serial port initialisation. Skip over these to
     * avoid failing our own allocations. Note, this value is just hardcoded
     * for the amount of slots this initialisation currently uses up.
     * JIRA: CAMKES-204.
     */
    free_slot_start += 16;

    /* We need to be able to actual store caps to the maximum number of objects
     * we may be dealing with.
     * This check can still pass and initialisation fail as we need extra slots for duplicates
     * for CNodes and TCBs.
     */
    assert(free_slot_end - free_slot_start >= CONFIG_CAPDL_LOADER_MAX_OBJECTS);

    ZF_LOGD("  %ld free cap slots, from %ld to %ld\n", (long)(free_slot_end - free_slot_start), (long)free_slot_start, (long)free_slot_end);

#if CONFIG_CAPDL_LOADER_PRINT_UNTYPEDS
    int num_untyped = bootinfo->untyped.end - bootinfo->untyped.start;
    ZF_LOGD("  Untyped memory (%d)\n", num_untyped);
    for (int i = 0; i < num_untyped; i++) {
        uintptr_t ut_paddr = bootinfo->untypedList[i].paddr;
        uintptr_t ut_size = bootinfo->untypedList[i].sizeBits;
        bool ut_isDevice = bootinfo->untypedList[i].isDevice;
        ZF_LOGD("    0x%016" PRIxPTR " - 0x%016" PRIxPTR " (%s)\n", ut_paddr,
                ut_paddr + BIT(ut_size), ut_isDevice ? "device" : "memory");
    }
#endif

    ZF_LOGD("Loader is running in domain %d\n", bootinfo->initThreadDomain);

    first_arm_iospace = bootinfo->ioSpaceCaps.start;
}

static int find_device_object(void *paddr, seL4_Word type, int size_bits, seL4_CPtr free_slot,
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
                obj->paddr == paddr &&
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
                    error = seL4_CNode_Move(seL4_CapInitThreadCNode, hold_slot, CONFIG_WORD_SIZE, seL4_CapInitThreadCNode, free_slot, CONFIG_WORD_SIZE);
                    ZF_LOGF_IFERR(error, "");
                }
            }
        }
    }
    return -1;
}

/* Create objects */
static int
retype_untyped(seL4_CPtr free_slot, seL4_CPtr free_untyped,
               seL4_ArchObjectType object_type, int object_size)
{
    seL4_CPtr root = seL4_CapInitThreadCNode;
    int node_index = 0;
    int node_depth = 0;
    int node_offset = free_slot;

    int no_objects = 1;

    int err = seL4_Untyped_Retype(free_untyped, object_type, object_size,
                                  root, node_index, node_depth, node_offset, no_objects);

    return err;
}

bool isDeviceObject(CDL_Object *obj)
{
    return (obj->paddr != NULL && (CDL_Obj_Type(obj) == CDL_Frame || CDL_Obj_Type(obj) == CDL_Untyped));
}

unsigned int
create_object(CDL_Model *spec, CDL_Object *obj, CDL_ObjID id, seL4_BootInfo *info, seL4_CPtr untyped_slot,
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
#ifdef CONFIG_KERNEL_RT
    case CDL_SchedContext:
        obj_size = kobject_get_size(KOBJECT_SCHED_CONTEXT, obj_size);
        obj_type = (seL4_ArchObjectType) CDL_Obj_Type(obj);
        break;
#endif
    default:
        obj_type = (seL4_ArchObjectType) CDL_Obj_Type(obj);
    }

    if (CDL_Obj_Type(obj) == CDL_CNode) {
        ZF_LOGD(" (CNode of size %d bits)", obj_size);
    }

    seL4_Error err = seL4_NoError;

#ifdef CONFIG_ARCH_X86
    if (CDL_Obj_Type(obj) == CDL_IOPorts) {
        err = seL4_X86_IOPortControl_Issue(seL4_CapIOPortControl, obj->start, obj->end, seL4_CapInitThreadCNode, free_slot, CONFIG_WORD_SIZE);
        ZF_LOGF_IF(err != seL4_NoError, "Failed to allocate IOPort for range [%d,%d]", (int)obj->start, (int)obj->end);
        return seL4_NoError;
    }
#endif

    if (isDeviceObject(obj)) {
        ZF_LOGD(" device frame/untyped, paddr = %p, size = %d bits\n", obj->paddr, obj_size);

        /* This is a device object. Look for it in bootinfo. */
        err = find_device_object(obj->paddr, obj_type, obj_size, free_slot, id, info, spec);
        ZF_LOGF_IF(err != seL4_NoError, "Failed to find device frame/untyped at paddr = %p\n", obj->paddr);
        return seL4_NoError;
    } else {
        return retype_untyped(free_slot, untyped_slot, obj_type, obj_size);
    }
}

static void
create_objects(CDL_Model *spec, seL4_BootInfo *bootinfo)
{
    ZF_LOGD("Creating objects...\n");

    unsigned int obj_id_index = 0;
    unsigned int free_slot_index = 0;
    unsigned int ut_index = 0;

    // Each time through the loop either:
    //  - we successfully create an object, and move to the next object to create
    //    OR
    //  - we fail to create an object, and move to the next untyped object

    while (obj_id_index < spec->num && ut_index < (bootinfo->untyped.end - bootinfo->untyped.start)) {
        CDL_ObjID obj_id = obj_id_index;
        seL4_CPtr free_slot = free_slot_start + free_slot_index;
        seL4_CPtr untyped_cptr = untyped_cptrs[ut_index];
        CDL_Object *obj = &spec->objects[obj_id_index];
        CDL_ObjectType capdl_obj_type = CDL_Obj_Type(obj);

        ZF_LOGV("Creating object %s in slot %ld, from untyped %lx...\n", CDL_Obj_Name(obj), (long)free_slot,
                (long)untyped_cptr);

        switch (capdl_obj_type) {
        case CDL_Interrupt:
#ifdef CONFIG_ARCH_X86
        case CDL_IODevice:
        case CDL_IOAPICInterrupt:
        case CDL_MSIInterrupt:
#else
        case CDL_ARMIODevice:
#endif
            // Never create Interrupt objects here
            break;
        default: {
            /* at this point we are definately creating an object - figure out what it is */
            seL4_Error err = create_object(spec, obj, obj_id, bootinfo, untyped_cptr, free_slot);
            if (err == seL4_NoError) {
                if (capdl_obj_type == CDL_ASIDPool) {
                    free_slot_index++;
                    seL4_CPtr asid_slot = free_slot_start + free_slot_index;
                    err = seL4_ARCH_ASIDControl_MakePool(seL4_CapASIDControl, free_slot,
                                                         seL4_CapInitThreadCNode, asid_slot, CONFIG_WORD_SIZE);
                    free_slot = asid_slot;
                    ZF_LOGF_IFERR(err, "Failed to create asid pool");
                }
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
        }
        obj_id_index++;
    }
    // Update the free slot to go past all the objects we just made.
    free_slot_start += free_slot_index;

    if (obj_id_index != spec->num) {
        /* We didn't iterate through all the objects. */
        ZF_LOGF("Ran out of untyped memory while creating objects.");
    }
}

static void
create_irq_cap(CDL_IRQ irq, CDL_Object *obj, seL4_CPtr free_slot)
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
#endif
    default:
        error = seL4_IRQControl_Get(seL4_CapIRQControl, irq, root, index, depth);
    }
    ZF_LOGF_IFERR(error, "Failed to create irq cap");
    add_sel4_cap(irq, IRQ, index);
}

static void
create_irq_caps(CDL_Model *spec)
{
    ZF_LOGD("Creating irq handler caps...\n");

    for (CDL_IRQ irq = 0; irq < CONFIG_CAPDL_LOADER_MAX_IRQS; irq++) {
        if (spec->irqs[irq] != INVALID_OBJ_ID) {
            seL4_CPtr free_slot = get_free_slot();

            ZF_LOGD(" Creating irq handler cap for IRQ %d...\n", irq);
            create_irq_cap(irq, &spec->objects[spec->irqs[irq]], free_slot);
            next_free_slot();
        }
    }
}

/* Mint a cap that will not be given to the user */
/* Used for badging fault eps in the RT kernel */
static void
mint_cap(CDL_ObjID object_id, int free_slot, seL4_Word badge)
{
    seL4_CapRights_t rights = seL4_AllRights;

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
static void
duplicate_cap(CDL_ObjID object_id, int free_slot)
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

static void
duplicate_caps(CDL_Model *spec)
{
    ZF_LOGD("Duplicating CNodes...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode || spec->objects[obj_id].type == CDL_TCB) {
            ZF_LOGD(" Duplicating %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            int free_slot = get_free_slot();
            duplicate_cap(obj_id, free_slot);
            next_free_slot();
        }
    }
}

static void
create_sched_ctrl_caps(seL4_BootInfo *bi)
{
#ifdef CONFIG_KERNEL_RT
    for (seL4_Word i = 0; i <= bi->schedcontrol.end - bi->schedcontrol.start; i++) {
        add_sel4_cap(i, SCHED_CTRL, i + bi->schedcontrol.start);
    }
#endif
}

/* Initialise SCs */
static void
init_sc(CDL_Model *spec, CDL_ObjID sc, CDL_Core affinity)
{
    CDL_Object *cdl_sc = get_spec_object(spec, sc);

    uint64_t UNUSED budget = CDL_SC_Budget(cdl_sc);
    uint64_t UNUSED period = CDL_SC_Period(cdl_sc);
    seL4_Word UNUSED data = CDL_SC_Data(cdl_sc);

    ZF_LOGD("budget: %llu, period: %llu, data: %u\n", budget, period, data);

    seL4_CPtr UNUSED seL4_sc = orig_caps(sc);
    seL4_CPtr UNUSED sched_control = sched_ctrl_caps(affinity);
#ifdef CONFIG_KERNEL_RT
    /* Assign the sched context to run on the CPU that the root task runs on. */
    int error = seL4_SchedControl_Configure(sched_control,
                                            seL4_sc, budget, period, 0, data);
    ZF_LOGF_IFERR(error, "");
#endif
}

/* Initialise TCBs */
static void
init_tcb(CDL_Model *spec, CDL_ObjID tcb)
{
    CDL_Object *cdl_tcb = get_spec_object(spec, tcb);

    CDL_Cap *cdl_cspace_root = get_cap_at(cdl_tcb, CDL_TCB_CTable_Slot);
#ifndef CONFIG_CAPDL_LOADER_ALLOW_NO_CSPACE
    if (cdl_cspace_root == NULL) {
        ZF_LOGF("Could not find CSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }
#endif
    CDL_Cap *cdl_vspace_root = get_cap_at(cdl_tcb, CDL_TCB_VTable_Slot);
    if (cdl_vspace_root == NULL) {
        ZF_LOGF("Could not find VSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }
    CDL_Cap *cdl_ipcbuffer   = get_cap_at(cdl_tcb, CDL_TCB_IPCBuffer_Slot);
    if (cdl_ipcbuffer == NULL) {
        ZF_LOGD("  Warning: TCB has no IPC buffer\n");
    }

    CDL_Cap *cdl_sc   = get_cap_at(cdl_tcb, CDL_TCB_SC_Slot);

    seL4_Word ipcbuffer_addr = CDL_TCB_IPCBuffer_Addr(cdl_tcb);
    uint8_t priority = CDL_TCB_Priority(cdl_tcb);
    CDL_Core UNUSED affinity = CDL_TCB_Affinity(cdl_tcb);
    uint8_t UNUSED max_priority = CDL_TCB_MaxPriority(cdl_tcb);

    seL4_CPtr sel4_tcb = orig_caps(tcb);

    seL4_CPtr sel4_cspace_root = cdl_cspace_root == NULL ? 0 : orig_caps(CDL_Cap_ObjID(cdl_cspace_root));
    seL4_CPtr sel4_vspace_root = orig_caps(CDL_Cap_ObjID(cdl_vspace_root));
    seL4_CPtr sel4_ipcbuffer   = cdl_ipcbuffer ? orig_caps(CDL_Cap_ObjID(cdl_ipcbuffer)) : 0;
    seL4_CPtr UNUSED sel4_sc   = cdl_sc ? orig_caps(CDL_Cap_ObjID(cdl_sc)) : 0;

    seL4_CPtr sel4_fault_ep;
    seL4_CPtr UNUSED sel4_tempfault_ep;
    seL4_CPtr badged_sel4_fault_ep;

    if (config_set(CONFIG_KERNEL_RT)) {
        /* Fault ep cptrs are in the caller's cspace */

        CDL_Cap *cdl_fault_ep   = get_cap_at(cdl_tcb, CDL_TCB_FaultEP_Slot);
        if (cdl_fault_ep == NULL) {
            ZF_LOGW("  Warning: TCB has no fault endpoint\n");
        }

        CDL_Cap *cdl_tempfault_ep   = get_cap_at(cdl_tcb, CDL_TCB_TemporalFaultEP_Slot);
        if (cdl_tempfault_ep == NULL) {
            ZF_LOGW("  Warning: TCB has no temporal fault endpoint\n");
        }

        sel4_fault_ep = cdl_fault_ep ? orig_caps(CDL_Cap_ObjID(cdl_fault_ep)) : 0;
        sel4_tempfault_ep = cdl_tempfault_ep ? orig_caps(CDL_Cap_ObjID(cdl_tempfault_ep)) : 0;

        if (sel4_fault_ep != 0) {
            seL4_Word fault_ep_badge = get_capData(CDL_Cap_Data(cdl_fault_ep));
            if (fault_ep_badge != 0) {
                badged_sel4_fault_ep = (seL4_CPtr) get_free_slot();
                mint_cap(CDL_Cap_ObjID(cdl_fault_ep), badged_sel4_fault_ep,
                                       fault_ep_badge);
                next_free_slot();
                sel4_fault_ep = badged_sel4_fault_ep;

            }
        }
    } else {
        /* Fault ep cptrs are in the configured thread's cspace */
        sel4_fault_ep = cdl_tcb->tcb_extra.fault_ep;
    }

    seL4_Word sel4_cspace_root_data = seL4_NilData;
    if (cdl_cspace_root != NULL) {
        sel4_cspace_root_data = get_capData(CDL_Cap_Data(cdl_cspace_root));
    }
    seL4_Word sel4_vspace_root_data = get_capData(CDL_Cap_Data(cdl_vspace_root));

    int error;
#ifdef CONFIG_KERNEL_RT
    if (sel4_sc) {
        init_sc(spec, CDL_Cap_ObjID(cdl_sc), affinity);
    }

    error = seL4_TCB_Configure(sel4_tcb, sel4_fault_ep, sel4_tempfault_ep,
                               sel4_sc,
                               sel4_cspace_root, sel4_cspace_root_data,
                               sel4_vspace_root, sel4_vspace_root_data,
                               ipcbuffer_addr, sel4_ipcbuffer);
#else
    error = seL4_TCB_Configure(sel4_tcb, sel4_fault_ep,
                               sel4_cspace_root, sel4_cspace_root_data,
                               sel4_vspace_root, sel4_vspace_root_data,
                               ipcbuffer_addr, sel4_ipcbuffer);
    ZF_LOGF_IFERR(error, "");

#endif

    error = seL4_TCB_SetSchedParams(sel4_tcb, seL4_CapInitThreadTCB, max_priority, priority);
    ZF_LOGF_IFERR(error, "");

#if CONFIG_MAX_NUM_NODES > 1
    error = seL4_TCB_SetAffinity(sel4_tcb, affinity);
#endif

    ZF_LOGF_IFERR(error, "");

#ifdef SEL4_DEBUG_KERNEL
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

static void
configure_tcb(CDL_Model *spec, CDL_ObjID tcb)
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
            *(seL4_Word*)(copy_addr_with_pt + sp % PAGE_SIZE_4K) = argv[i];
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
        .r0 = argc > 0 ? argv[0] : 0,
        .r1 = argc > 1 ? argv[1] : 0,
        .r2 = argc > 2 ? argv[2] : 0,
        .r3 = argc > 3 ? argv[3] : 0,
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
#endif
    };
    ZF_LOGD("  Setting up _start(");
    for (int i = 0; i < argc; i++) {
        ZF_LOGD("%p", (void*)argv[i]);
        if (i != argc - 1) {
            ZF_LOGD(", ");
        }
    }
    ZF_LOGD(")...\n");
    ZF_LOGD("pc = %p\n", (void*)pc);
    ZF_LOGD("sp = %p\n", (void*)sp);

    global_user_context = regs;

    int error = seL4_TCB_WriteRegisters(sel4_tcb, false, 0,
                                        sizeof(seL4_UserContext) / sizeof(seL4_Word),
                                        &global_user_context);
    ZF_LOGF_IFERR(error, "");

    uint32_t UNUSED domain = CDL_TCB_Domain(cdl_tcb);
    ZF_LOGD("  Assigning thread to domain %u...\n", domain);
    error = seL4_DomainSet_Set(seL4_CapDomain, domain, sel4_tcb);
    ZF_LOGF_IFERR(error, "");
}

static void
init_tcbs(CDL_Model *spec)
{
    ZF_LOGD("Initialising TCBs...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            ZF_LOGD(" Initialising %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_tcb(spec, obj_id);

            ZF_LOGD(" Configuring %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            configure_tcb(spec, obj_id);
        }
    }
}

static void
init_elf(CDL_Model *spec, CDL_ObjID tcb, seL4_BootInfo *bootinfo)
{
    CDL_Object *cdl_tcb = get_spec_object(spec, tcb);

    CDL_Cap *cdl_vspace_root = get_cap_at(cdl_tcb, CDL_TCB_VTable_Slot);
    if (cdl_vspace_root == NULL) {
        ZF_LOGF("Could not find VSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }

    elf_load_frames(CDL_TCB_ElfName(cdl_tcb), CDL_Cap_ObjID(cdl_vspace_root), spec, bootinfo);
}

static void
init_elfs(CDL_Model *spec, seL4_BootInfo *bootinfo)
{
    ZF_LOGD("Initialising ELFs...\n");
    ZF_LOGD(" Available ELFs:\n");
    for (int j = 0; ; j++) {
        const char *name = NULL;
        unsigned long size;
        void *ptr = cpio_get_entry(_capdl_archive, j, &name, &size);
        if (ptr == NULL) {
            break;
        }
        ZF_LOGD("  %d: %s, offset: %p, size: %lu\n", j, name,
                (void*)((uintptr_t)ptr - (uintptr_t)_capdl_archive), size);
    }
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            ZF_LOGD(" Initialising ELF for %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_elf(spec, obj_id, bootinfo);
        }
    }
}

static void
init_irq(CDL_Model *spec, CDL_IRQ irq_no)
{
    seL4_CPtr irq_handler_cap = irq_caps(irq_no);

    CDL_Object *cdl_irq = get_spec_object(spec, spec->irqs[irq_no]);
#ifdef CONFIG_ARCH_X86
    assert(cdl_irq->type == CDL_Interrupt || cdl_irq->type == CDL_IOAPICInterrupt || cdl_irq->type == CDL_MSIInterrupt);
#else
    assert(cdl_irq->type == CDL_Interrupt);
#endif
    assert(cdl_irq != NULL);

    if (cdl_irq->size_bits != 0) {
        ZF_LOGF("Misconfigured IRQ; an IRQ must have a size of 0.\n");
    }
    if (cdl_irq->slots.num > 1) {
        ZF_LOGF("Misconfigured IRQ; an IRQ cannot have more than one assigned endpoint.\n");
    }

    if (cdl_irq->slots.num == 1) {
        /* This IRQ is bound. */
        CDL_Cap *endpoint_cap = &cdl_irq->slots.slot[0].cap;
        seL4_CPtr endpoint_cptr = orig_caps(CDL_Cap_ObjID(endpoint_cap));

        int error = seL4_IRQHandler_SetNotification (irq_handler_cap, endpoint_cptr);
        ZF_LOGF_IFERR(error, "");
    }
}

static void
init_irqs(CDL_Model *spec)
{
    ZF_LOGD("Initialising IRQ handler caps...\n");

    for (CDL_IRQ irq = 0; irq < CONFIG_CAPDL_LOADER_MAX_IRQS; irq++) {
        if (spec->irqs[irq] != INVALID_OBJ_ID) {
            ZF_LOGD(" Initialising handler for IRQ %d...\n", irq);
            init_irq(spec, irq);
        }
    }
}

/* Initialise virtual address spaces */
static void
set_asid(CDL_Model *spec UNUSED, CDL_ObjID page)
{
    seL4_CPtr sel4_page = orig_caps(page);
    int error = seL4_ARCH_ASIDPool_Assign(seL4_CapInitThreadASIDPool, sel4_page);
    ZF_LOGF_IFERR(error, "");
}

static void
init_pd_asids(CDL_Model *spec)
{
    ZF_LOGD("Initialising Page Directory ASIDs...\n");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        CDL_ObjectType type;
#ifdef CONFIG_ARCH_X86_64
        type = CDL_PML4;
#else
        type = CDL_PD;
#endif
        if (spec->objects[obj_id].type == type) {
            ZF_LOGD(" Initialising pd/pml4 ASID %s...\n",
                    CDL_Obj_Name(&spec->objects[obj_id]));
            set_asid(spec, obj_id);
        }
    }
}

static void
map_page(CDL_Model *spec UNUSED, CDL_Cap *page_cap, CDL_ObjID pd_id,
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
    ZF_LOGD("   Mapping %s into %s with rights={G: %d, R: %d, W: %d}, vaddr=0x%x, vm_attribs=0x%x\n",
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
                ZF_LOGE("%p", (void*)addr.paddr);
            }
            ZF_LOGE(" -> %p (error = %d)\n", (void*)vaddr, error);
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
        if (!(vm_attribs & seL4_ARM_PageCacheable) && spec->objects[page].paddr == NULL) {
            seL4_Word size_bits = spec->objects[page].size_bits;
            assert(size_bits <= sizeof(uintptr_t) * CHAR_BIT - 1 && "illegal object size");
            error = seL4_ARM_Page_CleanInvalidate_Data(sel4_page, 0, BIT(size_bits));
            ZF_LOGF_IFERR(error, "");
        }
#endif
    } else {
        ZF_LOGF("attempt to map something that is not a frame or PT");
    }
}

#ifdef CONFIG_ARCH_X86_64
static void
init_pt(CDL_Model *spec, CDL_ObjID pml4, uintptr_t pt_base, CDL_ObjID pt)
{
    CDL_Object *obj = get_spec_object(spec, pt);
    for (unsigned long slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = pt_base + (obj_slot << (seL4_PageBits));
        CDL_Cap *frame_cap = CDL_CapSlot_Cap(slot);
        seL4_CapRights_t frame_rights = CDL_seL4_Cap_Rights(frame_cap);
        map_page(spec, frame_cap, pml4, frame_rights, base);
    }
}

static void
init_pd(CDL_Model *spec, CDL_ObjID pml4, uintptr_t pd_base, CDL_ObjID pd)
{
    CDL_Object *obj = get_spec_object(spec, pd);
    for (unsigned long slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = pd_base + (obj_slot << (seL4_PageTableIndexBits + seL4_PageBits));
        CDL_Cap *pt_cap = CDL_CapSlot_Cap(slot);
        CDL_ObjID pt_obj = CDL_Cap_ObjID(pt_cap);
        if (CDL_Cap_Type(pt_cap) == CDL_FrameCap) {
            seL4_CapRights_t frame_rights = CDL_seL4_Cap_Rights(pt_cap);
            map_page(spec, pt_cap, pml4, frame_rights, base);
        } else {
            seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(pt_cap);
            seL4_X86_PageTable_Map(orig_caps(pt_obj), orig_caps(pml4), base, vm_attribs);
            init_pt(spec, pml4, base, pt_obj);
        }
    }
}

static void
init_pdpt(CDL_Model *spec, CDL_ObjID pml4, uintptr_t pdpt_base, CDL_ObjID pdpt)
{
    CDL_Object *obj = get_spec_object(spec, pdpt);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = pdpt_base + (obj_slot << (seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits));
        CDL_Cap *pd_cap = CDL_CapSlot_Cap(slot);
        CDL_ObjID pd_obj = CDL_Cap_ObjID(pd_cap);
        if (CDL_Cap_Type(pd_cap) == CDL_FrameCap) {
            seL4_CapRights_t frame_rights = CDL_seL4_Cap_Rights(pd_cap);
            map_page(spec, pd_cap, pml4, frame_rights, base);
        } else {
            seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(pd_cap);
            seL4_X86_PageDirectory_Map(orig_caps(pd_obj), orig_caps(pml4), base, vm_attribs);
            init_pd(spec, pml4, base, pd_obj);
        }
    }
}

static void
init_pml4(CDL_Model *spec, CDL_ObjID pml4)
{
    CDL_Object *obj = get_spec_object(spec, pml4);
    for (unsigned long slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++) {
        CDL_CapSlot *slot = CDL_Obj_GetSlot(obj, slot_index);
        unsigned long obj_slot = CDL_CapSlot_Slot(slot);
        uintptr_t base = obj_slot << (seL4_PDPTIndexBits + seL4_PageDirIndexBits + seL4_PageTableIndexBits + seL4_PageBits);
        CDL_Cap *pdpt_cap = CDL_CapSlot_Cap(slot);
        CDL_ObjID pdpt_obj = CDL_Cap_ObjID(pdpt_cap);
        seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(pdpt_cap);
        seL4_X86_PDPT_Map(orig_caps(pdpt_obj), orig_caps(pml4), base, vm_attribs);
        init_pdpt(spec, pml4, base, pdpt_obj);
    }
}

#else

static void
map_page_directory_slot(CDL_Model *spec UNUSED, CDL_ObjID pd_id, CDL_CapSlot *pd_slot)
{
    ZF_LOGD("  Mapping slot %d in %s\n", pd_slot->slot, CDL_Obj_Name(&spec->objects[pd_id]));
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pd_slot);

    seL4_Word page_vaddr = CDL_CapSlot_Slot(pd_slot) << (seL4_PageTableIndexBits + seL4_PageBits);
    seL4_CapRights_t page_rights = CDL_seL4_Cap_Rights(page_cap);

    map_page(spec, page_cap, pd_id, page_rights, page_vaddr);
}

static void
map_page_directory(CDL_Model *spec, CDL_ObjID pd_id)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd_id);

    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++) {
        map_page_directory_slot(spec, pd_id, CDL_Obj_GetSlot(cdl_pd, slot_index));
    }
}

static void
map_page_table_slot(CDL_Model *spec UNUSED, CDL_ObjID pd, CDL_ObjID pt UNUSED,
                    seL4_Word pt_vaddr, CDL_CapSlot *pt_slot)
{
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pt_slot);

    seL4_Word page_vaddr = pt_vaddr + (CDL_CapSlot_Slot(pt_slot) << seL4_PageBits);
    seL4_CapRights_t page_rights = CDL_seL4_Cap_Rights(page_cap);

    ZF_LOGD("  Mapping %s into %s[%d] with rights={G: %d, R: %d, W: %d}, vaddr=0x%" PRIxPTR "\n",
            CDL_Obj_Name(&spec->objects[pt]), CDL_Obj_Name(&spec->objects[pd]), pt_slot->slot,
            seL4_CapRights_get_capAllowGrant(page_rights),
            seL4_CapRights_get_capAllowRead(page_rights),
            seL4_CapRights_get_capAllowWrite(page_rights),
            (uintptr_t)pt_vaddr);

    map_page(spec, page_cap, pd, page_rights, page_vaddr);
}

static void
map_page_table_slots(CDL_Model *spec, CDL_ObjID pd, CDL_CapSlot *pd_slot)
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

static void
map_page_directory_page_tables(CDL_Model *spec, CDL_ObjID pd)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++) {
        map_page_table_slots(spec, pd, CDL_Obj_GetSlot(cdl_pd, slot_index));
    }
}
#endif

static void
init_vspace(CDL_Model *spec)
{
    ZF_LOGD("Initialising VSpaces...\n");

#ifdef CONFIG_ARCH_X86_64
    /* Have no understanding of the logic of model of whatever the hell the
       other code in this function is doing as it is pure gibberish. For
       x86_64 we will just do the obvious recursive initialization */
    ZF_LOGD("================================\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PML4) {
            ZF_LOGD(" Initialising pml4 %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_pml4(spec, obj_id);
        }
    }
#else
    ZF_LOGD("================================\n");
    ZF_LOGD("Initialising page directories...\n");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PD) {
            ZF_LOGD(" Initialising page directory %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            map_page_directory(spec, obj_id);
        }
    }

    ZF_LOGD("===========================\n");
    ZF_LOGD("Initialising page tables...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PD) {
            ZF_LOGD(" Initialising page tables in %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
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
static void
init_cnode_slot(CDL_Model *spec, init_cnode_mode mode, CDL_ObjID cnode_id, CDL_CapSlot *cnode_slot)
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
    case CDL_SchedControlCap:
        src_index = sched_ctrl_caps(CDL_SchedControl_Core(get_spec_object(spec, target_cap_obj)));
        break;
    default:
        src_index = orig_caps(target_cap_obj);
        break;
    }

    uint8_t src_depth = CONFIG_WORD_SIZE;

    if (mode == MOVE && move_cap) {
        if (is_ep_cap || is_irq_handler_cap) {
            ZF_LOGD("moving...\n");
            int error = seL4_CNode_Move(dest_root, dest_index, dest_depth,
                                        src_root, src_index, src_depth);
            ZF_LOGF_IFERR(error, "");
        } else {
            ZF_LOGD("mutating (with badge/guard %p)...\n", (void*)target_cap_data);
            int error = seL4_CNode_Mutate(dest_root, dest_index, dest_depth,
                                          src_root, src_index, src_depth, target_cap_data);
            ZF_LOGF_IFERR(error, "");
        }
    } else if (mode == COPY && !move_cap) {
        if (is_frame_cap && target_cap->mapping_container_id != INVALID_OBJ_ID) {
            /* The spec requires the frame cap in the current slot be the same one
             * used to perform the mapping of the frame in some container (either
             * a page table or page directory). */
            CDL_ObjID container_id = target_cap->mapping_container_id;
            seL4_Word slot_index = target_cap->mapping_slot;

            /* Look up the container object which contains the mapping. */
            CDL_Object *container = get_spec_object(spec, container_id);
            assert(container);
            assert(container->type == CDL_PT || container->type == CDL_PD);

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
            ZF_LOGD("minting (with badge/guard %p)...\n", (void*)target_cap_data);
            int error = seL4_CNode_Mint(dest_root, dest_index, dest_depth,
                                        src_root, src_index, src_depth, target_cap_rights, target_cap_data);
            ZF_LOGF_IFERR(error, "");
        }
    } else {
        ZF_LOGD("skipping\n");
    }
}

static void
init_cnode(CDL_Model *spec, init_cnode_mode mode, CDL_ObjID cnode)
{
    CDL_Object *cdl_cnode = get_spec_object(spec, cnode);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_cnode); slot_index++) {
        if (CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.type == CDL_IRQHandlerCap) {
            CDL_IRQ UNUSED irq = CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.irq;
            ZF_LOGD("  Populating slot %d with cap to IRQ %d, name %s...\n",
                    CDL_Obj_GetSlot(cdl_cnode, slot_index)->slot, irq,
                    CDL_Obj_Name(&spec->objects[spec->irqs[irq]]));
        } else {
            ZF_LOGD("  Populating slot %d with cap to %s...\n",
                    CDL_Obj_GetSlot(cdl_cnode, slot_index)->slot,
                    CDL_Obj_Name(&spec->objects[CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.obj_id]));
        }
        init_cnode_slot(spec, mode, cnode, CDL_Obj_GetSlot(cdl_cnode, slot_index));
    }
}

static void
init_cspace(CDL_Model *spec)
{
    ZF_LOGD("Copying Caps...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode) {
            ZF_LOGD(" Copying into %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_cnode(spec, COPY, obj_id);
        }
    }

    ZF_LOGD("Moving Caps...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode) {
            ZF_LOGD(" Moving into %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_cnode(spec, MOVE, obj_id);
        }
    }
}

static void
start_threads(CDL_Model *spec)
{
    ZF_LOGD("Starting threads...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            ZF_LOGD(" Starting %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            seL4_CPtr tcb = orig_caps(obj_id);
            int error = seL4_TCB_Resume(tcb);
            ZF_LOGF_IFERR(error, "");
        }
    }
}

static void
init_fill_frames(CDL_Model *spec, simple_t * simple)
{
    seL4_Word i;
    for (i = 0; i < spec->num_frame_fill; i++) {
        /* get the cap to the original object */
        seL4_CPtr cap = capdl_to_sel4_orig[spec->frame_fill[i].frame];
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

        /* Determine destination */
        uintptr_t dest = base + spec->frame_fill[i].dest_offset;
        ssize_t max = BIT(spec->objects[spec->frame_fill[i].frame].size_bits) - spec->frame_fill[i].dest_offset;
        if (max < 0) {
            max = 0;
        }
        /* Check for which type */
        if (strcmp(spec->frame_fill[i].type, "bootinfo") == 0) {
            /* Ask simple to fill it in */
            int id;
            if (strcmp(spec->frame_fill[i].extra_information, "SEL4_BOOTINFO_HEADER_X86_VBE") == 0) {
                id = SEL4_BOOTINFO_HEADER_X86_VBE;
            } else if (strcmp(spec->frame_fill[i].extra_information, "SEL4_BOOTINFO_HEADER_X86_TSC_FREQ") == 0) {
                id = SEL4_BOOTINFO_HEADER_X86_TSC_FREQ;
            } else {
                ZF_LOGF("Unable to parse extra information for \"bootinfo\", given \"%s\"", spec->frame_fill[i].extra_information);
            }
            error = simple_get_extended_bootinfo(simple, id, (void*)dest, max);
            if (error == -1) {
                seL4_BootInfoHeader empty = (seL4_BootInfoHeader) {
                    .id = -1, .len = -1
                };
                memcpy((void*)dest, &empty, MIN(max, sizeof(empty)));
            }
        } else {
            ZF_LOGF("Unsupported frame fill type %s", spec->frame_fill[i].type);
        }

        /* Unmap the frame */
        error = seL4_ARCH_Page_Unmap(cap);
        ZF_LOGF_IFERR(error, "");
    }
}

static void
init_scs(CDL_Model *spec)
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

static void
init_system(CDL_Model *spec)
{
    seL4_BootInfo *bootinfo = platsupport_get_bootinfo();
    simple_t simple;

    simple_default_init_bootinfo(&simple, bootinfo);

    init_copy_frame(bootinfo);

    parse_bootinfo(bootinfo);
    sort_untypeds(bootinfo);

    create_objects(spec, bootinfo);
    create_irq_caps(spec);
    if (config_set(CONFIG_KERNEL_RT)) {
        create_sched_ctrl_caps(bootinfo);
    }
    duplicate_caps(spec);

    init_irqs(spec);
    init_pd_asids(spec);
    init_elfs(spec, bootinfo);
    init_fill_frames(spec, &simple);
    init_vspace(spec);
    init_scs(spec);
    init_tcbs(spec);
    init_cspace(spec);
    start_threads(spec);
}

int
main(void)
{
#ifdef CONFIG_CAPDL_LOADER_PRINTF
    /* Allow us to print via seL4_Debug_PutChar. */
    platsupport_serial_setup_bootinfo_failsafe();
#endif

    ZF_LOGD("Starting Loader...\n");
    init_system(&capdl_spec);

    ZF_LOGD("We used %d CSlots (%.2LF%% of our CNode)\n", get_free_slot(),
            (long double)get_free_slot() /
            (long double)(BIT(CONFIG_ROOT_CNODE_SIZE_BITS) * 100));
    ZF_LOGD(A_RESET A_FG_G "Done; suspending..." A_RESET "\n");
    seL4_TCB_Suspend(seL4_CapInitThreadTCB);
}
