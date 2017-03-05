/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
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
#ifdef CONFIG_KERNEL_STABLE
#include <simple-stable/simple-stable.h>
#endif //CONFIG_KERNEL_STABLE

#include <utils/util.h>
#include <sel4/sel4.h>

#include "debug.h"
#include "capdl.h"

#include "capdl_spec.h"

#define PML4_SLOT(vaddr) ((vaddr >> (PDPT_SIZE + PD_SIZE + PT_SIZE + FRAME_SIZE)) & (( 1 << PML4_SIZE) - 1))
#define PDPT_SLOT(vaddr) ((vaddr >> (PD_SIZE + PT_SIZE + FRAME_SIZE)) & (( 1 << PDPT_SIZE) - 1))
#define PD_SLOT(vaddr)   ((vaddr >> (PT_SIZE + FRAME_SIZE)) & (( 1 << PD_SIZE) - 1))
#define PT_SLOT(vaddr)   ((vaddr >> FRAME_SIZE) & ((1 << PT_SIZE) - 1))

#define ANSI_RESET "\033[0m"
#define ANSI_GREEN   ANSI_RESET "\033[32m"

#ifndef CONFIG_CAPDL_LOADER_PRINTF
    #define debug_printf(...) do { } while (0)

#else
    #define debug_printf(args...) printf(args)

#endif

#define CAPDL_SHARED_FRAMES

#define STACK_ALIGNMENT_BYTES 16

static seL4_CPtr capdl_to_sel4_orig[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_copy[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_irq[CONFIG_CAPDL_LOADER_MAX_OBJECTS];

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

#ifdef CONFIG_KERNEL_STABLE
int seL4_Untyped_Retype(seL4_Untyped service, int type, int size_bits, seL4_CNode root, int node_index, int node_depth, int node_offset, int num_objects)
{
    /* Cache the last untyped retype to 'optimise' the search. This is not
     * strictly speaking an optimization, as it is required for correctness.
     * If the capdl loader is attempting to allocate an untyped to give to
     * a loaded process, then it is important that we do not create other
     * objects at the same location as that untyped. However, in the stable
     * kernel it is permissable to use untyped cap X to create child untyped
     * Y, and then create some other object (say a frame) at the same location
     * as Y by invoking cap X again. */
    static seL4_Untyped last_service = 0;
    static int next_offset = 0;

    int offset = 0;
    int ret;

    int memory_size_bits = vka_get_object_size(type, size_bits);
    assert(memory_size_bits > 0);

    /* check if we match the cached offset */
    if (service == last_service) {
        /* Round the cached offset up to be aligned to the current size */
        offset = (( (next_offset - 1) >> memory_size_bits) + 1) << memory_size_bits;

        ret = seL4_Untyped_RetypeAtOffset(service, type, offset, size_bits, root, node_index,
                node_depth, node_offset, num_objects);

        next_offset = offset + (1 << memory_size_bits);
        return ret;
    }

    /* didn't match cached offset - start searching from the start */
    for (offset = 0, ret = seL4_RevokeFirst; ret == seL4_RevokeFirst; offset += (1 << memory_size_bits)) {
        ret = seL4_Untyped_RetypeAtOffset(service, type, offset, size_bits, root, node_index,
                node_depth, node_offset, num_objects);
    }
    last_service = service;
    next_offset = offset + (1 << memory_size_bits);

    return ret;
}
#endif //CONFIG_KERNEL_STABLE

static seL4_CPtr
get_free_slot(void)
{
    return free_slot_start;
}

static void
next_free_slot(void)
{
    free_slot_start++;
    if (free_slot_start >= free_slot_end)
        die("Ran out of free slots!");
}

typedef enum {MOVE, COPY} init_cnode_mode;
typedef enum {ORIG, DUP, IRQ} seL4_cap_type;

static seL4_CPtr
orig_caps(CDL_ObjID object_id) {
    assert(object_id < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_orig[object_id];
}

static seL4_CPtr
dup_caps(CDL_ObjID object_id) {
    assert(object_id < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_copy[object_id];
}

static seL4_CPtr
irq_caps(CDL_IRQ irq) {
    assert(irq < CONFIG_CAPDL_LOADER_MAX_OBJECTS);
    return capdl_to_sel4_irq[irq];
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
    }
}

static CDL_Object
*get_spec_object(CDL_Model *spec, CDL_ObjID object_id)
{
    return &spec->objects[object_id];
}

static seL4_CapData_t
get_capData(CDL_CapData d)
{
    switch (d.tag) {
        case CDL_CapData_Badge:
            return seL4_CapData_Badge_new(d.badge);
        case CDL_CapData_Guard:
            return seL4_CapData_Guard_new(d.guard_bits, d.guard_size);
        case CDL_CapData_Raw:
            return (seL4_CapData_t){{d.data}};
        default:
            die("invalid cap data");
            return (seL4_CapData_t){{0}};
    }
}

static CDL_Cap *
get_cap_at(CDL_Object *obj, unsigned int slot)
{
    for (unsigned int i = 0; i < CDL_Obj_NumSlots(obj); i++) {
        CDL_CapSlot *s = CDL_Obj_GetSlot(obj, i);
        if (CDL_CapSlot_Slot(s) == slot)
            return CDL_CapSlot_Cap(s);
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
        die("Could not find PD cap %s[%d]", CDL_Obj_Name(cdl_pml4), (int)PML4_SLOT(vaddr));
    }
    return pdpt_cap;
}

static CDL_Cap *get_cdl_frame_pd(CDL_ObjID root, uintptr_t vaddr, CDL_Model *spec)
{
    CDL_Cap *pdpt_cap = get_cdl_frame_pdpt(root, vaddr, spec);
    CDL_Object *cdl_pdpt = get_spec_object(spec, CDL_Cap_ObjID(pdpt_cap));
    CDL_Cap *pd_cap = get_cap_at(cdl_pdpt, PDPT_SLOT(vaddr));
    if (pd_cap == NULL) {
        die("Could not find PD cap %s[%d]", CDL_Obj_Name(cdl_pdpt), (int)PDPT_SLOT(vaddr));
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
        die("Could not find PT cap %s[%d]", CDL_Obj_Name(cdl_pd), (int)PD_SLOT(vaddr));
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
        die("Could not find frame cap %s[%d]", CDL_Obj_Name(cdl_pt), (int)PT_SLOT(vaddr));
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

static seL4_ArchObjectType
seL4_frame_type(int size)
{
    switch (size) {
#ifdef CONFIG_ARCH_ARM
        case seL4_PageBits:
            return seL4_ARM_SmallPageObject;
        case seL4_LargePageBits:
            return seL4_ARM_LargePageObject;
        case seL4_SectionBits:
            return seL4_ARM_SectionObject;
        case seL4_SuperSectionBits:
            return seL4_ARM_SuperSectionObject;
#elif defined(CONFIG_ARCH_X86)
        case seL4_PageBits:
            return seL4_X86_4K;
        case seL4_LargePageBits:
            return seL4_X86_LargePageObject;
#endif
        default:
            die("illegal frame size");
    }
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
#ifdef CONFIG_KERNEL_STABLE
    seL4_CPtr copy_addr_pt = bootinfo->userImagePTs.start +
            PD_SLOT(((uintptr_t)copy_addr)) - PD_SLOT(((uintptr_t)&__executable_start));
#else
    seL4_CPtr copy_addr_pt = bootinfo->userImagePaging.start + 1 +
        PD_SLOT(((uintptr_t)copy_addr)) - PD_SLOT(((uintptr_t)&__executable_start));
#endif
#ifdef CONFIG_ARCH_X6_64
    /* guess that there is one PDPT and PML4 */
    copy_addr_pt += 2;
#endif

    int error;

    for (int i = 0; i < sizeof(copy_addr_with_pt) / PAGE_SIZE_4K; i++) {
#ifdef CONFIG_ARCH_ARM
        error = seL4_ARM_Page_Unify_Instruction(copy_addr_frame + i, 0, PAGE_SIZE_4K);
        seL4_AssertSuccess(error);
#endif
        error = seL4_ARCH_Page_Unmap(copy_addr_frame + i);
        seL4_AssertSuccess(error);

        if ((i + 1) % (1 << PT_SIZE) == 0) {
            error = seL4_ARCH_PageTable_Unmap(copy_addr_pt + i / (1 << PT_SIZE));
            seL4_AssertSuccess(error);
        }
    }
}

static void
elf_load_frames(const char *elf_name, CDL_ObjID pd, CDL_Model *spec,
        seL4_BootInfo *bootinfo)
{
    unsigned long elf_size;
    void *elf_file = cpio_get_file(_capdl_archive,elf_name,&elf_size);

    if (elf_file == NULL) {
        die("ELF file %s not found", elf_name);
    }

    if (elf_checkFile(elf_file) != 0)
        die("Unable to read elf file %s at %p", elf_name, elf_file);

    debug_printf("   ELF loading %s (from %p)... \n", elf_name, elf_file);

    for (int i = 0; i < elf_getNumProgramHeaders(elf_file); i++) {
        debug_printf("    to %p... ", (void*)(uintptr_t)elf_getProgramHeaderVaddr(elf_file, i));

        size_t f_len = elf_getProgramHeaderFileSize(elf_file, i);
        uintptr_t dest = elf_getProgramHeaderVaddr(elf_file, i);
        uintptr_t src = (uintptr_t) elf_file + elf_getProgramHeaderOffset(elf_file, i);

        //Skip non loadable headers
        if(elf_getProgramHeaderType(elf_file, i) != PT_LOAD) {
            debug_printf("Skipping non loadable header\n");
            continue;
        }
        uintptr_t vaddr = dest;

        while (vaddr < dest + f_len) {
            debug_printf(".");

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
                seL4_AssertSuccess(error);
                error = seL4_ARCH_Page_Map(sel4_page, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                    seL4_ReadWrite, attribs);
            }
            if (error) {
                /* Try and retrieve some useful information to help the user
                 * diagnose the error.
                 */
                debug_printf("Failed to map frame ");
                seL4_ARCH_Page_GetAddress_t addr UNUSED = seL4_ARCH_Page_GetAddress(sel4_page);
                if (addr.error) {
                    debug_printf("<unknown physical address (error = %d)>", addr.error);
                } else {
                    debug_printf("%p", (void*)addr.paddr);
                }
                debug_printf(" -> %p (error = %d)\n", (void*)copy_addr, error);
                seL4_AssertSuccess(error);
            }

            /* copy until end of section or end of page */
            size_t len = dest + f_len - vaddr;
            if (len > sel4_page_size - (vaddr % sel4_page_size)) {
                len = sel4_page_size - (vaddr % sel4_page_size);
            }
            memcpy((void *) (copy_addr + vaddr % sel4_page_size), (void *) (src + vaddr - dest), len);

#ifdef CONFIG_ARCH_ARM
            error = seL4_ARM_Page_Unify_Instruction(sel4_page, 0, sel4_page_size);
            seL4_AssertSuccess(error);
#endif
            error = seL4_ARCH_Page_Unmap(sel4_page);
            seL4_AssertSuccess(error);

            if (sel4_page_pt != 0) {
                error = seL4_ARCH_PageTable_Unmap(sel4_page_pt);
                seL4_AssertSuccess(error);
            }

            vaddr += len;
        }

        /* Overwrite the section type so that next time this section is
         * encountered it will be skipped as it is not considered loadable. A
         * bit of a hack, but fine for now.
         */
        debug_printf(" Marking header as loaded\n");
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

    debug_printf("Sorting untypeds...\n");

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
            debug_printf("Untyped %3d (cptr=%p) (addr=%p) is of size %2d. Skipping as it is device\n",
                         untyped_index, (void*)(untyped_start + untyped_index),
                         (void*)bootinfo->untypedList[untyped_index].paddr,
                         bootinfo->untypedList[untyped_index].sizeBits);
        } else {
            debug_printf("Untyped %3d (cptr=%p) (addr=%p) is of size %2d. Placing in slot %d...\n",
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
    debug_printf("Parsing bootinfo...\n");

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

    debug_printf("  %ld free cap slots, from %ld to %ld\n", (long)(free_slot_end - free_slot_start), (long)free_slot_start, (long)free_slot_end);

#if CONFIG_CAPDL_LOADER_PRINT_UNTYPEDS
    int num_untyped = bootinfo->untyped.end - bootinfo->untyped.start;
    debug_printf("  Untyped memory (%d)\n", num_untyped);
    for (int i = 0; i < num_untyped; i++) {
        uintptr_t ut_paddr = bootinfo->untypedList[i].paddr;
        uintptr_t ut_size = bootinfo->untypedList[i].sizeBits;
        bool ut_isDevice = bootinfo->untypedList[i].isDevice;
        debug_printf("    0x%016" PRIxPTR " - 0x%016" PRIxPTR " (%s)\n", ut_paddr,
            ut_paddr + (1 << ut_size), ut_isDevice ? "device" : "memory");
    }
#endif

#if defined(CONFIG_KERNEL_MASTER) || defined(CONFIG_KERNEL_STABLE)
    debug_printf("Loader is running in domain %d\n", bootinfo->initThreadDomain);
#endif

#if CONFIG_CAPDL_LOADER_PRINT_DEVICE_INFO
#ifdef CONFIG_KERNEL_STABLE
    int num_device_untyped = bootinfo->deviceUntyped.end - bootinfo->deviceUntyped.start;
    int offset = bootinfo->untyped.end - bootinfo->untyped.start;
    debug_printf("  Device untyped memory (%d)\n", num_device_untyped);
    for (int i = 0; i < num_device_untyped; i++) {
        uintptr_t ut_paddr = bootinfo->untypedPaddrList[i + offset];
        uintptr_t ut_size = bootinfo->untypedSizeBitsList[i + offset];
        debug_printf("    0x%016" PRIxPTR " - 0x%016" PRIxPTR "\n", ut_paddr,
            ut_paddr + (1 << ut_size));
    }
#endif
#endif

    first_arm_iospace = bootinfo->ioSpaceCaps.start;
}

#ifdef CONFIG_KERNEL_STABLE
static int find_device_frame(void *paddr, int size_bits, seL4_CPtr free_slot, CDL_ObjID obj_id,
        seL4_BootInfo *bootinfo, CDL_Model *spec) {
    /* Construct a path with assumptions on a 1 level cspace as seL4 construct for a rootserver */
    cspacepath_t path = {
        .capPtr = free_slot,
        .capDepth = 32,
        .root = seL4_CapInitThreadCNode,
        .dest = 0,
        .destDepth = 0,
        .offset = free_slot,
        .window = 1
    };
    int error = simple_stable_get_frame_cap(bootinfo, paddr, size_bits, &path);
    seL4_AssertSuccess(error);
    add_sel4_cap(obj_id, ORIG, free_slot);
    return 0;
}
static int find_device_untyped(void *paddr, int obj_size, seL4_CPtr free_slot, CDL_ObjID obj_id, seL4_BootInfo *bootinfo, CDL_Model *spec) {
    seL4_CPtr untyped;
    seL4_Word offset;
    int error;
    simple_stable_get_frame_info(bootinfo, paddr, obj_size, &untyped, &offset);

    if (untyped == 0) {
        /* Failed to find an untyped */
        return -1;
    }
    error = seL4_Untyped_RetypeAtOffset(untyped, seL4_UntypedObject, offset, obj_size, seL4_CapInitThreadCNode, 0, 0, free_slot, 1);
    if (error == seL4_NoError) {
        add_sel4_cap(obj_id, ORIG, free_slot);
    }
    return error;
}
#else
static int find_device_object(void *paddr, seL4_Word type, int size_bits, seL4_CPtr free_slot,
        CDL_ObjID obj_id, seL4_BootInfo *bootinfo, CDL_Model *spec) {
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
            seL4_AssertSuccess(error);
            add_sel4_cap(obj_id, ORIG, free_slot);
            return 0;
        }
    }
    /* Assume we are allocating from a device untyped. Do a linear search for it */
    for (unsigned int i = 0; i < bootinfo->untyped.end - bootinfo->untyped.start; i++) {
        if (bootinfo->untypedList[i].paddr <= (uintptr_t)paddr &&
            bootinfo->untypedList[i].paddr + BIT(bootinfo->untypedList[i].sizeBits) >= (uintptr_t)paddr + BIT(size_bits)) {
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
                if (error) {
                    return -1;
                }
                seL4_ARCH_Page_GetAddress_t addr UNUSED = seL4_ARCH_Page_GetAddress(free_slot);
                if (addr.error) {
                    /* if this fails assume it's an untyped and create a temporary frame in it
                     * to get the address from */
                    error = seL4_Untyped_Retype(free_slot, seL4_frame_type(seL4_PageBits), seL4_PageBits,
                                                seL4_CapInitThreadCNode, 0, 0, free_slot + 2, 1);
                    if (error) {
                        return -1;
                    }
                    addr = seL4_ARCH_Page_GetAddress(free_slot + 2);
                    error = seL4_CNode_Delete(seL4_CapInitThreadCNode, free_slot + 2, CONFIG_WORD_SIZE);
                    seL4_AssertSuccess(error);
                    if (addr.error) {
                        return -1;
                    }
                }
                if (addr.paddr == (uintptr_t)paddr) {
                    /* nailed it */
                    add_sel4_cap(obj_id, ORIG, free_slot);
                    /* delete any holding cap */
                    if (hold_slot) {
                        error = seL4_CNode_Delete(seL4_CapInitThreadCNode, hold_slot, CONFIG_WORD_SIZE);
                        seL4_AssertSuccess(error);
                    }
                    return 0;
                }
                if (addr.paddr > (uintptr_t)paddr) {
                    /* device frames probably not ordered by physical address */
                    return -1;
                }
                /* if we are currently using a hold slot we can just delete the cap, otherwise start the hold */
                if (hold_slot) {
                    error = seL4_CNode_Delete(seL4_CapInitThreadCNode, free_slot, CONFIG_WORD_SIZE);
                    seL4_AssertSuccess(error);
                } else {
                    hold_slot = free_slot + 1;
                    error = seL4_CNode_Move(seL4_CapInitThreadCNode, hold_slot, CONFIG_WORD_SIZE, seL4_CapInitThreadCNode, free_slot, CONFIG_WORD_SIZE);
                    seL4_AssertSuccess(error);
                }
            }
        }
    }
    return -1;
}

static int find_device_frame(void *paddr, int size_bits, seL4_CPtr free_slot, CDL_ObjID obj_id,
        seL4_BootInfo *bootinfo, CDL_Model *spec) {
    return find_device_object(paddr, seL4_frame_type(size_bits), size_bits, free_slot, obj_id, bootinfo, spec);
}

static int find_device_untyped(void *paddr, int size_bits, seL4_CPtr free_slot, CDL_ObjID obj_id,
        seL4_BootInfo *bootinfo, CDL_Model *spec) {
    return find_device_object(paddr, seL4_UntypedObject, size_bits, free_slot, obj_id, bootinfo, spec);
}
#endif

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

static void
create_objects(CDL_Model *spec, seL4_BootInfo *bootinfo)
{
    debug_printf("Creating objects...\n");

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
        seL4_ArchObjectType obj_type = seL4_ObjectTypeCount;
        int obj_size = 0;

#ifdef CONFIG_CAPDL_LOAD_PRINT_CAPDL_OBJECTS
        debug_printf("Creating object %s in slot %ld, from untyped %lx...\n", CDL_Obj_Name(obj), (long)free_slot, (long)untyped_cptr);
#endif

#ifdef CONFIG_ARCH_X86
        if (capdl_obj_type == CDL_IOPorts) {
            seL4_CPtr root = seL4_CapInitThreadCNode;
            int index = seL4_CapIOPort;
            int depth = CONFIG_WORD_SIZE;

            int err = seL4_CNode_Copy(root, free_slot, depth,
                root, index, depth, seL4_AllRights);
            seL4_AssertSuccess(err);

            add_sel4_cap(obj_id, ORIG, free_slot);

            obj_id_index++;
            free_slot_index++;
            continue;
        }
#endif

        if (capdl_obj_type == CDL_Frame && obj->paddr != NULL) {
            obj_size = CDL_Obj_SizeBits(obj);
            debug_printf(" device frame, paddr = %p, size = %d bits\n", obj->paddr, obj_size);

            /* This is a device frame. Look for it in bootinfo. */
            if (find_device_frame(obj->paddr, obj_size, free_slot, obj_id, bootinfo, spec) == 0) {
                /* We found and added the frame. */
                obj_id_index++;
                free_slot_index++;
                continue;
            }

            die("Failed to find device frame at paddr = %p\n", obj->paddr);
        }

        // Never create Interrupt objects here
#ifdef CONFIG_ARCH_X86
        if (capdl_obj_type == CDL_Interrupt || capdl_obj_type == CDL_IOPorts || capdl_obj_type == CDL_IODevice || capdl_obj_type == CDL_IOAPICInterrupt || capdl_obj_type == CDL_MSIInterrupt) {
#else
        if (capdl_obj_type == CDL_Interrupt || capdl_obj_type == CDL_ARMIODevice) {
#endif
            obj_id_index++;
        } else {
            obj_size = CDL_Obj_SizeBits(obj);
            if (capdl_obj_type == CDL_CNode) {
                debug_printf(" (CNode of size %d bits)\n", obj_size);
            }

            // CapDL types are not the same as seL4 types,
            // as seL4 needs different types for different frame sizes.
            if (capdl_obj_type == CDL_Frame) {
                obj_type = seL4_frame_type (obj_size);
#ifndef CONFIG_KERNEL_STABLE
            } else if (capdl_obj_type == CDL_ASIDPool) {
                obj_type = CDL_Untyped;
                obj_size = 12;
#endif
            } else {
                obj_type = (seL4_ArchObjectType) capdl_obj_type;
            }
        }

        if (capdl_obj_type == CDL_Untyped && obj->paddr != NULL) {
            debug_printf(" device untyped, paddr = %p, size_bits = %d\n", obj->paddr, obj_size);

            /* This is a device untyped. Look for it in bootinfo. */
            if (find_device_untyped(obj->paddr, obj_size, free_slot, obj_id, bootinfo, spec) == seL4_NoError) {
                /* We found and added the frame. */
                obj_id_index++;
                free_slot_index++;
                continue;
            }

            die("Failed to find device untyped at paddr = %p, size_bits = %d\n", obj->paddr, obj_size);
        }

        // Create object
#ifdef CONFIG_ARCH_X86
        if (capdl_obj_type != CDL_Interrupt && capdl_obj_type != CDL_IOPorts && capdl_obj_type != CDL_IODevice && capdl_obj_type != CDL_IOAPICInterrupt && capdl_obj_type != CDL_MSIInterrupt) {
#else
        if (capdl_obj_type != CDL_Interrupt && capdl_obj_type != CDL_ARMIODevice) {
#endif
            int err = retype_untyped(free_slot, untyped_cptr, obj_type, obj_size);

            if (err == seL4_NoError) {
#ifndef CONFIG_KERNEL_STABLE
                if (capdl_obj_type == CDL_ASIDPool) {
                    free_slot_index++;
                    seL4_CPtr asid_slot = free_slot_start + free_slot_index;
                    err = seL4_ARCH_ASIDControl_MakePool(seL4_CapASIDControl, free_slot, seL4_CapInitThreadCNode, asid_slot, CONFIG_WORD_SIZE);
                    seL4_AssertSuccess(err);
                    free_slot = asid_slot;
                }
#endif
                add_sel4_cap(obj_id, ORIG, free_slot);

                obj_id_index++;
                free_slot_index++;
            } else if (err == seL4_NotEnoughMemory) {
                ut_index++;
            } else {
                /* Exit with failure. */
                seL4_AssertSuccess(err);
            }
        }
    }
    // Update the free slot to go past all the objects we just made.
    free_slot_start += free_slot_index;

    if (obj_id_index != spec->num) {
        /* We didn't iterate through all the objects. */
        die("Ran out of untyped memory while creating objects.");
    }
}

static void
create_irq_cap(CDL_IRQ irq, CDL_Object *obj, seL4_CPtr free_slot)
{
    seL4_CPtr root = seL4_CapInitThreadCNode;
    int index = free_slot;
    int depth = CONFIG_WORD_SIZE;
    int error;

#if defined(CONFIG_ARCH_X86) && !defined(CONFIG_KERNEL_STABLE)
    if (CDL_Obj_Type(obj) == CDL_IOAPICInterrupt) {
        error = seL4_IRQControl_GetIOAPIC(seL4_CapIRQControl, root, index, depth, \
                      obj->ioapicirq_extra.ioapic, obj->ioapicirq_extra.ioapic_pin, \
                      obj->ioapicirq_extra.level, obj->ioapicirq_extra.polarity, \
                      irq);
    } else if (CDL_Obj_Type(obj) == CDL_MSIInterrupt) {
        error = seL4_IRQControl_GetMSI(seL4_CapIRQControl, root, index, depth, \
                    obj->msiirq_extra.pci_bus, obj->msiirq_extra.pci_dev, \
                    obj->msiirq_extra.pci_fun, obj->msiirq_extra.handle, irq);
    } else {
#endif
        error = seL4_IRQControl_Get(seL4_CapIRQControl, irq, root, index, depth);
#if defined(CONFIG_ARCH_X86) && !defined(CONFIG_KERNEL_STABLE)
    }
#endif
    seL4_AssertSuccess(error);

    add_sel4_cap(irq, IRQ, index);
}

static void
create_irq_caps(CDL_Model *spec)
{
    debug_printf("Creating irq handler caps...\n");

    for (CDL_IRQ irq = 0; irq < CONFIG_CAPDL_LOADER_MAX_IRQS; irq++) {
        if (spec->irqs[irq] != INVALID_OBJ_ID) {
            seL4_CPtr free_slot = get_free_slot();

            debug_printf(" Creating irq handler cap for IRQ %d...\n", irq);
            create_irq_cap(irq, &spec->objects[spec->irqs[irq]], free_slot);
            next_free_slot();
        }
    }
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
    seL4_AssertSuccess(error);

    add_sel4_cap(object_id, DUP, dest_index);
}

static void
duplicate_caps(CDL_Model *spec)
{
    debug_printf("Duplicating CNodes...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode || spec->objects[obj_id].type == CDL_TCB) {
            debug_printf(" Duplicating %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            int free_slot = get_free_slot();
            duplicate_cap(obj_id, free_slot);
            next_free_slot();
        }
    }
}

/* Initialise SCs */
static void
init_sc(CDL_Model *spec, CDL_ObjID sc, UNUSED seL4_BootInfo *bi)
{
    CDL_Object *cdl_sc = get_spec_object(spec, sc);

    uint64_t UNUSED budget = CDL_SC_Budget(cdl_sc);
    uint64_t UNUSED period = CDL_SC_Period(cdl_sc);
    seL4_Word UNUSED data = CDL_SC_Data(cdl_sc);

    debug_printf("budget: %llu, period: %llu, data: %u\n", budget, period, data);

    seL4_CPtr UNUSED seL4_sc = orig_caps(sc);

#ifdef CONFIG_KERNEL_RT
    assert(bi->schedcontrol.start != seL4_CapNull);
    /* Assign the sched context to run on the CPU that the root task runs on. */
    int error = seL4_SchedControl_Configure(bi->schedcontrol.start + bi->nodeID,
                                            seL4_sc, budget, period, 0);
    seL4_AssertSuccess(error);
#endif
}

static void
init_scs(CDL_Model *spec, seL4_BootInfo *bi)
{
    debug_printf("Initialising SCs...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_SchedContext) {
            debug_printf(" Initialising %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_sc(spec, obj_id, bi);
        }
    }
}

/* Initialise TCBs */
static void
init_tcb(CDL_Model *spec, CDL_ObjID tcb)
{
    CDL_Object *cdl_tcb = get_spec_object(spec, tcb);

    CDL_Cap *cdl_cspace_root = get_cap_at(cdl_tcb, CDL_TCB_CTable_Slot);
#ifndef CONFIG_CAPDL_LOADER_ALLOW_NO_CSPACE
    if (cdl_cspace_root == NULL) {
        die("Could not find CSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }
#endif
    CDL_Cap *cdl_vspace_root = get_cap_at(cdl_tcb, CDL_TCB_VTable_Slot);
    if (cdl_vspace_root == NULL) {
        die("Could not find VSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }
    CDL_Cap *cdl_ipcbuffer   = get_cap_at(cdl_tcb, CDL_TCB_IPCBuffer_Slot);
    if (cdl_ipcbuffer == NULL) {
        debug_printf("  Warning: TCB has no IPC buffer\n");
    }

    CDL_Cap *cdl_sc   = get_cap_at(cdl_tcb, CDL_TCB_SC_Slot);

    seL4_Word ipcbuffer_addr = CDL_TCB_IPCBuffer_Addr(cdl_tcb);
    uint8_t priority = CDL_TCB_Priority(cdl_tcb);
    uint8_t UNUSED affinity = CDL_TCB_Affinity(cdl_tcb);
    uint8_t UNUSED max_priority = CDL_TCB_MaxPriority(cdl_tcb);
    uint8_t UNUSED criticality = CDL_TCB_Criticality(cdl_tcb);
    uint8_t UNUSED max_criticality = CDL_TCB_MaxCriticality(cdl_tcb);

    seL4_CPtr sel4_tcb = orig_caps(tcb);

    seL4_CPtr sel4_cspace_root = cdl_cspace_root == NULL ? 0 : orig_caps(CDL_Cap_ObjID(cdl_cspace_root));
    seL4_CPtr sel4_vspace_root = orig_caps(CDL_Cap_ObjID(cdl_vspace_root));
    seL4_CPtr sel4_ipcbuffer   = cdl_ipcbuffer ? orig_caps(CDL_Cap_ObjID(cdl_ipcbuffer)) : 0;
    seL4_CPtr UNUSED sel4_sc   = cdl_sc ? orig_caps(CDL_Cap_ObjID(cdl_sc)) : 0;

    seL4_CPtr sel4_fault_ep;

    /* Fault ep cptrs are in the configured thread's cspace */
    sel4_fault_ep = cdl_tcb->tcb_extra.fault_ep;

    seL4_CapData_t sel4_cspace_root_data = cdl_cspace_root == NULL ? (seL4_CapData_t){{0}} : get_capData(CDL_Cap_Data(cdl_cspace_root));
    seL4_CapData_t sel4_vspace_root_data = get_capData(CDL_Cap_Data(cdl_vspace_root));

    int error;
#ifdef CONFIG_KERNEL_RT
    /* There was originally a notion of criticality in the RT API;
     *
     * During some design iterations, it was removed from exposure to userspace,
     * and had to be hidden. Eventually it will be re-added. When that happens,
     * just add it back here (or wherever else would go best, assuming the
     * re-added criticality concept isn't the same).
     */
    seL4_PrioProps_t prio;
    seL4_PrioProps_ptr_new(&prio, (seL4_Uint32) max_priority, (seL4_Uint32) priority);

    error = seL4_TCB_Configure(sel4_tcb, sel4_fault_ep,
                               prio, sel4_sc,
                               sel4_cspace_root, sel4_cspace_root_data,
                               sel4_vspace_root, sel4_vspace_root_data,
                               ipcbuffer_addr, sel4_ipcbuffer);
#else
    error = seL4_TCB_Configure(sel4_tcb, sel4_fault_ep, seL4_PrioProps_new(priority, priority),
                               sel4_cspace_root, sel4_cspace_root_data,
                               sel4_vspace_root, sel4_vspace_root_data,
                               ipcbuffer_addr, sel4_ipcbuffer);
    seL4_AssertSuccess(error);

#if CONFIG_MAX_NUM_NODES > 1
    error = seL4_TCB_SetAffinity(sel4_tcb, affinity);
#endif

#endif
    seL4_AssertSuccess(error);

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
        die("TCB %s's stack pointer is not dword-aligned", CDL_Obj_Name(&spec->objects[tcb]));
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
        die("TCB %s has more than four arguments, which is not supported using"
            " the register calling convention", CDL_Obj_Name(&spec->objects[tcb]));
#else //!CONFIG_CAPDL_LOADER_CC_REGISTERS
        /* We need to map the TCB's stack into our address space because there
         * are arguments to write.
         */

        /* Find the TCB's PD. */
        CDL_Cap *cdl_vspace_root = get_cap_at(cdl_tcb, CDL_TCB_VTable_Slot);
        CDL_ObjID pd = CDL_Cap_ObjID(cdl_vspace_root);

        if (STACK_ALIGNMENT_BYTES % sizeof(*argv)) {
            die("Stack alignment requirement not evenly divisible by argument size");
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
        seL4_AssertSuccess(error);

        /* Write all necessary arguments to the TCB's stack. */
        for (int i = argc - 1; i >= 0 && i >= reg_args; i--) {
            if (i != argc - 1 && sp % PAGE_SIZE_4K == 0) {
                /* We could support this case with more complicated logic, but
                 * choose not to.
                 */
                die("TCB %s's initial arguments cause its stack to cross a page boundary",
                    CDL_Obj_Name(&spec->objects[tcb]));
            }
            sp -= sizeof(seL4_Word);
            *(seL4_Word*)(copy_addr_with_pt + sp % PAGE_SIZE_4K) = argv[i];
        }

#ifdef CONFIG_ARCH_ARM
        error = seL4_ARM_Page_Unify_Instruction(frame, 0, PAGE_SIZE_4K);
        seL4_AssertSuccess(error);
#endif //CONFIG_ARCH_ARM
        error = seL4_ARCH_Page_Unmap(frame);
        seL4_AssertSuccess(error);
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
    debug_printf("  Setting up _start(");
    for (int i = 0; i < argc; i++) {
        debug_printf("%p", (void*)argv[i]);
        if (i != argc - 1) {
            debug_printf(", ");
        }
    }
    debug_printf(")...\n");
    debug_printf("pc = %p\n", (void*)pc);
    debug_printf("sp = %p\n", (void*)sp);

    global_user_context = regs;

    int error = seL4_TCB_WriteRegisters(sel4_tcb, false, 0,
                                        sizeof(seL4_UserContext) / sizeof(seL4_Word),
                                        &global_user_context);
    seL4_AssertSuccess(error);

    uint32_t UNUSED domain = CDL_TCB_Domain(cdl_tcb);
    debug_printf("  Assigning thread to domain %u...\n", domain);
#if defined(CONFIG_KERNEL_MASTER) || defined(CONFIG_KERNEL_STABLE)
    error = seL4_DomainSet_Set(seL4_CapDomain, domain, sel4_tcb);
    seL4_AssertSuccess(error);
#endif
}

static void
init_tcbs(CDL_Model *spec)
{
    debug_printf("Initialising TCBs...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            debug_printf(" Initialising %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_tcb(spec, obj_id);

            debug_printf(" Configuring %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
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
        die("Could not find VSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }

    elf_load_frames(CDL_TCB_ElfName(cdl_tcb), CDL_Cap_ObjID(cdl_vspace_root), spec, bootinfo);
}

static void
init_elfs(CDL_Model *spec, seL4_BootInfo *bootinfo)
{
    debug_printf("Initialising ELFs...\n");
    debug_printf(" Available ELFs:\n");
    for (int j = 0; ; j++) {
        const char *name = NULL;
        unsigned long size;
        void *ptr = cpio_get_entry(_capdl_archive, j, &name, &size);
        if (ptr == NULL) {
            break;
        }
        debug_printf("  %d: %s, offset: %p, size: %lu\n", j, name,
            (void*)((uintptr_t)ptr - (uintptr_t)_capdl_archive), size);
    }
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            debug_printf(" Initialising ELF for %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
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
        die("Misconfigured IRQ; an IRQ must have a size of 0.\n");
    }
    if (cdl_irq->slots.num > 1) {
        die("Misconfigured IRQ; an IRQ cannot have more than one assigned endpoint.\n");
    }

    if (cdl_irq->slots.num == 1) {
        /* This IRQ is bound. */
        CDL_Cap *endpoint_cap = &cdl_irq->slots.slot[0].cap;
        seL4_CPtr endpoint_cptr = orig_caps(CDL_Cap_ObjID(endpoint_cap));

        int error = seL4_IRQHandler_SetNotification (irq_handler_cap, endpoint_cptr);
        seL4_AssertSuccess(error);
    }
}

static void
init_irqs(CDL_Model *spec)
{
    debug_printf("Initialising IRQ handler caps...\n");

    for (CDL_IRQ irq = 0; irq < CONFIG_CAPDL_LOADER_MAX_IRQS; irq++) {
        if (spec->irqs[irq] != INVALID_OBJ_ID) {
            debug_printf(" Initialising handler for IRQ %d...\n", irq);
            init_irq(spec, irq);
        }
    }
}

#ifndef CONFIG_KERNEL_STABLE
/* Initialise virtual address spaces */
static void
set_asid(CDL_Model *spec UNUSED, CDL_ObjID page)
{
    seL4_CPtr sel4_page = orig_caps(page);
    int error = seL4_ARCH_ASIDPool_Assign(seL4_CapInitThreadASIDPool, sel4_page);
    seL4_AssertSuccess(error);
}
#endif //!CONFIG_KERNEL_STABLE

static void
init_pd_asids(CDL_Model *spec)
{
#ifndef CONFIG_KERNEL_STABLE
    debug_printf("Initialising Page Directory ASIDs...\n");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
#ifdef CONFIG_ARCH_X86_64
        if (spec->objects[obj_id].type == CDL_PML4) {
            debug_printf(" Initialising pml4 ASID %s...\n",
                         CDL_Obj_Name(&spec->objects[obj_id]));
            set_asid(spec, obj_id);
        }
#else
        if (spec->objects[obj_id].type == CDL_PD) {
            debug_printf(" Initialising page directory ASID %s...\n",
                         CDL_Obj_Name(&spec->objects[obj_id]));
            set_asid(spec, obj_id);
        }
#endif
    }
#endif //!CONFIG_KERNEL_STABLE
}

static void
map_page(CDL_Model *spec UNUSED, CDL_Cap *page_cap, CDL_ObjID pd_id,
         seL4_CapRights_t rights, seL4_Word vaddr)
{
    CDL_ObjID page = CDL_Cap_ObjID(page_cap);

    // TODO: We should not be using the original cap here
    seL4_CPtr sel4_page = orig_caps(page);
    seL4_CPtr sel4_pd = orig_caps(pd_id);

    seL4_ARCH_VMAttributes vm_attribs = CDL_Cap_VMAttributes(page_cap);
    debug_printf("   Mapping %s into %s with rights={G: %d, R: %d, W: %d}, vaddr=0x%x, vm_attribs=0x%x\n",
                  CDL_Obj_Name(&spec->objects[page]),
                  CDL_Obj_Name(&spec->objects[pd_id]),
                  seL4_CapRights_get_capAllowGrant(rights),
                  seL4_CapRights_get_capAllowRead(rights),
                  seL4_CapRights_get_capAllowWrite(rights),
                  vaddr, vm_attribs);

    if (CDL_Cap_Type(page_cap) == CDL_PTCap) {
        int error = seL4_ARCH_PageTable_Map(sel4_page, sel4_pd, vaddr, vm_attribs);
        seL4_AssertSuccess(error);

    } else if (CDL_Cap_Type(page_cap) == CDL_FrameCap) {
#ifdef CAPDL_SHARED_FRAMES
        /* hack to support shared frames: create a new cap for each mapping */
        int dest_index = get_free_slot();

        int error_0 = seL4_CNode_Copy(seL4_CapInitThreadCNode, dest_index, CONFIG_WORD_SIZE,
                                      seL4_CapInitThreadCNode, sel4_page, CONFIG_WORD_SIZE, seL4_AllRights);
        seL4_AssertSuccess(error_0);

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
            debug_printf("Failed to map frame ");
            seL4_ARCH_Page_GetAddress_t addr UNUSED = seL4_ARCH_Page_GetAddress(sel4_page);
            if (addr.error) {
                debug_printf("<unknown physical address (error = %d)>", addr.error);
            } else {
                debug_printf("%p", (void*)addr.paddr);
            }
            debug_printf(" -> %p (error = %d)\n", (void*)vaddr, error);
            seL4_AssertSuccess(error);
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
            seL4_AssertSuccess(error);
        }
#endif
    } else {
        die("attempt to map something that is not a frame or PT");
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
        uintptr_t base = pt_base + (obj_slot << (FRAME_SIZE));
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
        uintptr_t base = pd_base + (obj_slot << (PT_SIZE + FRAME_SIZE));
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
        uintptr_t base = pdpt_base + (obj_slot << (PD_SIZE + PT_SIZE + FRAME_SIZE));
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
        uintptr_t base = obj_slot << (PDPT_SIZE + PD_SIZE + PT_SIZE + FRAME_SIZE);
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
    debug_printf("  Mapping slot %d in %s\n", pd_slot->slot, CDL_Obj_Name(&spec->objects[pd_id]));
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pd_slot);

    seL4_Word page_vaddr = CDL_CapSlot_Slot(pd_slot) << (PT_SIZE + FRAME_SIZE);
    seL4_CapRights_t page_rights = CDL_seL4_Cap_Rights(page_cap);

    map_page(spec, page_cap, pd_id, page_rights, page_vaddr);
}

static void
map_page_directory(CDL_Model *spec, CDL_ObjID pd_id)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd_id);

    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++)
        map_page_directory_slot(spec, pd_id, CDL_Obj_GetSlot(cdl_pd, slot_index));
}

static void
map_page_table_slot(CDL_Model *spec UNUSED, CDL_ObjID pd, CDL_ObjID pt UNUSED,
                    seL4_Word pt_vaddr, CDL_CapSlot *pt_slot)
{
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pt_slot);

    seL4_Word page_vaddr = pt_vaddr + (CDL_CapSlot_Slot(pt_slot) << FRAME_SIZE);
    seL4_CapRights_t page_rights = CDL_seL4_Cap_Rights(page_cap);

    debug_printf("  Mapping %s into %s[%d] with rights={G: %d, R: %d, W: %d}, vaddr=0x%" PRIxPTR "\n",
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

    seL4_Word page_vaddr = CDL_CapSlot_Slot(pd_slot) << (PT_SIZE + FRAME_SIZE);

    if (CDL_Cap_Type(page_cap) == CDL_PTCap) {
        CDL_Object *obj = get_spec_object(spec, page);
        for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(obj); slot_index++)
            map_page_table_slot(spec, pd, page, page_vaddr, CDL_Obj_GetSlot(obj, slot_index));
    }
}

static void
map_page_directory_page_tables(CDL_Model *spec, CDL_ObjID pd)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++)
        map_page_table_slots(spec, pd, CDL_Obj_GetSlot(cdl_pd, slot_index));
}
#endif

static void
init_vspace(CDL_Model *spec)
{
    debug_printf("Initialising VSpaces...\n");

#ifdef CONFIG_ARCH_X86_64
    /* Have no understanding of the logic of model of whatever the hell the
       other code in this function is doing as it is pure gibberish. For
       x86_64 we will just do the obvious recursive initialization */
    debug_printf("================================\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PML4) {
            debug_printf(" Initialising pml4 %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_pml4(spec, obj_id);
        }
    }
#else
    debug_printf("================================\n");
    debug_printf("Initialising page directories...\n");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PD) {
            debug_printf(" Initialising page directory %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            map_page_directory(spec, obj_id);
        }
    }

    debug_printf("===========================\n");
    debug_printf("Initialising page tables...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PD) {
            debug_printf(" Initialising page tables in %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            map_page_directory_page_tables(spec, obj_id);
        }
    }
#endif
}

static bool ep_related_cap(CDL_CapType cap) {
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
    seL4_CapData_t target_cap_data = get_capData(CDL_Cap_Data(target_cap));

    /* To support moving original caps, we need a spec with a CDT (most don't).
     * This shoud probably become a separate configuration option for when to
     * use the CDT, and when to just copy. For now, let's just copy.
     */
    bool move_cap = false; //FIXME
    bool is_ep_cap = ep_related_cap(target_cap_type);
    bool is_irq_handler_cap = (target_cap_type == CDL_IRQHandlerCap);
    bool is_frame_cap = (target_cap_type == CDL_FrameCap);
#ifdef CONFIG_ARCH_X86
    bool is_ioport_cap = (target_cap_type == CDL_IOPortsCap);
    bool is_iospace_cap = (target_cap_type == CDL_IOSpaceCap);
#endif
#ifdef CONFIG_ARCH_ARM
    bool is_iospace_cap = (target_cap_type == CDL_ARMIOSpaceCap);
#endif

    CDL_Object *dest_obj = get_spec_object(spec, cnode_id);
    uint8_t dest_size = CDL_Obj_SizeBits(dest_obj);

    // Use a copy of the cap to reference the destination, in case the original has already been moved.
    seL4_CPtr dest_root = dup_caps(cnode_id);
    int dest_index = CDL_CapSlot_Slot(cnode_slot);
    uint8_t dest_depth = dest_size;

    // Use an original cap to reference the object to copy.
    seL4_CPtr src_root = seL4_CapInitThreadCNode;
    int src_index;
#ifdef CONFIG_ARCH_X86
    if (is_ioport_cap) {
        src_index = seL4_CapIOPort;
    } else if (is_iospace_cap) {
        src_index = seL4_CapIOSpace;
    }
#elif defined(CONFIG_ARCH_ARM)
    if (is_iospace_cap) {
        src_index = first_arm_iospace + target_cap_data.words[0];
        target_cap_data = (seL4_CapData_t){0};
    }
#endif
    else if (is_irq_handler_cap) {
        src_index = irq_caps(target_cap_irq);
    } else {
        src_index = orig_caps(target_cap_obj);
    }

    uint8_t src_depth = CONFIG_WORD_SIZE;

    if (mode == MOVE && move_cap) {
        if (is_ep_cap || is_irq_handler_cap) {
            debug_printf("moving...\n");
            int error = seL4_CNode_Move(dest_root, dest_index, dest_depth,
                                        src_root, src_index, src_depth);
            seL4_AssertSuccess(error);
        } else {
            debug_printf("mutating (with badge/guard %p)...\n", (void*)target_cap_data.words[0]);
            int error = seL4_CNode_Mutate(dest_root, dest_index, dest_depth,
                                          src_root, src_index, src_depth, target_cap_data);
            seL4_AssertSuccess(error);
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
            seL4_AssertSuccess(error);
        } else {
          debug_printf("minting (with badge/guard %p)...\n", (void*)target_cap_data.words[0]);
          int error = seL4_CNode_Mint(dest_root, dest_index, dest_depth,
                                      src_root, src_index, src_depth, target_cap_rights, target_cap_data);
          seL4_AssertSuccess(error);
        }
    } else {
        debug_printf("skipping\n");
    }
}

static void
init_cnode(CDL_Model *spec, init_cnode_mode mode, CDL_ObjID cnode)
{
    CDL_Object *cdl_cnode = get_spec_object(spec, cnode);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_cnode); slot_index++) {
        if (CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.type == CDL_IRQHandlerCap) {
            CDL_IRQ UNUSED irq = CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.irq;
            debug_printf("  Populating slot %d with cap to IRQ %d, name %s...\n",
                         CDL_Obj_GetSlot(cdl_cnode, slot_index)->slot, irq,
                         CDL_Obj_Name(&spec->objects[spec->irqs[irq]]));
        } else {
            debug_printf("  Populating slot %d with cap to %s...\n",
                         CDL_Obj_GetSlot(cdl_cnode, slot_index)->slot,
                         CDL_Obj_Name(&spec->objects[CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.obj_id]));
        }
        init_cnode_slot(spec, mode, cnode, CDL_Obj_GetSlot(cdl_cnode, slot_index));
    }
}

static void
init_cspace(CDL_Model *spec)
{
    debug_printf("Copying Caps...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode) {
            debug_printf(" Copying into %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_cnode(spec, COPY, obj_id);
        }
    }

    debug_printf("Moving Caps...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_CNode) {
            debug_printf(" Moving into %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_cnode(spec, MOVE, obj_id);
        }
    }
}

static void
start_threads(CDL_Model *spec)
{
    debug_printf("Starting threads...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            debug_printf(" Starting %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            seL4_CPtr tcb = orig_caps(obj_id);
            int error = seL4_TCB_Resume(tcb);
            seL4_AssertSuccess(error);
        }
    }
}

static void
init_system(CDL_Model *spec)
{
    seL4_BootInfo *bootinfo = seL4_GetBootInfo();

    init_copy_frame(bootinfo);

    parse_bootinfo(bootinfo);
    sort_untypeds(bootinfo);

    create_objects(spec, bootinfo);
    create_irq_caps(spec);
    duplicate_caps(spec);

    init_irqs(spec);
    init_pd_asids(spec);
    init_elfs(spec, bootinfo);
    init_vspace(spec);
    if (config_set(CONFIG_KERNEL_RT)) {
        init_scs(spec, bootinfo);
    }
    init_tcbs(spec);
    init_cspace(spec);
    start_threads(spec);
}

int
main(void)
{
    /* Allow us to print via seL4_Debug_PutChar. */
    platsupport_serial_setup_bootinfo_failsafe();

    debug_printf("Starting Loader...\n");
    init_system(&capdl_spec);

    debug_printf("We used %d CSlots (%.2LF%% of our CNode)\n", get_free_slot(),
        (long double)get_free_slot() /
        (long double)(1U << CONFIG_ROOT_CNODE_SIZE_BITS) * 100);
    debug_printf(ANSI_GREEN "Done; suspending..." ANSI_RESET "\n");
    seL4_TCB_Suspend(seL4_CapInitThreadTCB);
}
