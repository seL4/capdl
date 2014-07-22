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

#ifdef CONFIG_CAPDL_LOADER_VERIFIED
#define NDEBUG

#ifdef CONFIG_KERNEL_STABLE
#error Verified stable kernel configuration is not supported
#endif

#endif

#include <assert.h>

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <elf/elf.h>
#include <sel4platsupport/platsupport.h>
#include <cpio/cpio.h>
#ifdef CONFIG_KERNEL_STABLE
#include <simple-stable/simple-stable.h>
#endif //CONFIG_KERNEL_STABLE
#endif //!CONFIG_CAPDL_LOADER_VERIFIED

#include <utils/util.h>
#include <sel4/sel4.h>

#include "debug.h"
#include "capdl.h"

#include "capdl_spec.h"
#include <syscall_stubs_sel4.h>

#define PD_SLOT(vaddr)   (vaddr >> (PT_SIZE + FRAME_SIZE))
#define PT_SLOT(vaddr)   ((vaddr >> FRAME_SIZE) & ((1 << PT_SIZE) - 1))

#define ANSI_RESET "\033[0m"
#define ANSI_GREEN   ANSI_RESET "\033[32m"
#define ANSI_DARK        ANSI_RESET "\033[30;1m"
#define ANSI_DARKBLUE    ANSI_RESET "\033[34;1m"

/*
 * Debugging information (disabled on verified builds).
 *
 * debug_printf prints the function and line numbers.
 * debug_printf_ is just printf.
 * debug_printfn prints the function name (disabled on low verbosity settings)
 */

#define verbose 1

#if defined(CONFIG_CAPDL_LOADER_VERIFIED) || !defined(CONFIG_CAPDL_LOADER_PRINTF)
    #define debug_printf(...)
    #define debug_printf_(...)
    #define debug_printfn(...)

#else /* !defined(CONFIG_CAPDL_LOADER_VERIFIED) && defined(CONFIG_CAPDL_LOADER_PRINTF) */
    #define debug_printf(fmt, ...) printf(ANSI_DARK "%25s:%4d \t" ANSI_RESET fmt, __FUNCTION__, __LINE__, ##__VA_ARGS__)

    #define debug_printf_(fmt, ...) printf(fmt, ##__VA_ARGS__)

    #if verbose > 1
        #define debug_printfn(fmt, ...) printf(ANSI_DARKBLUE "%25s:%4d \t" fmt ANSI_RESET, __FUNCTION__, __LINE__, ##__VA_ARGS__)
    #else /* verbose < 1 */
        #define debug_printfn(...)
    #endif /* verbose > 1 */

#endif /* defined(CONFIG_CAPDL_LOADER_VERIFIED) || !defined(CONFIG_CAPDL_LOADER_PRINTF) */

#if !defined(CONFIG_CAPDL_LOADER_VERIFIED)
    #define CAPDL_SHARED_FRAMES
#endif /* !defined(CONFIG_CAPDL_LOADER_VERIFIED) */

MUSLC_SYSCALL_TABLE;

static seL4_CPtr capdl_to_sel4_orig[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_copy[CONFIG_CAPDL_LOADER_MAX_OBJECTS];
static seL4_CPtr capdl_to_sel4_irq[CONFIG_CAPDL_LOADER_MAX_OBJECTS];

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
// List of untyped cptrs, sorted from largest to smallest.
static seL4_CPtr untyped_cptrs[CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS];
#endif


static seL4_CPtr free_slot_start, free_slot_end;

// Hack for seL4_TCB_WriteRegisters because we can't take the address of local variables.
static seL4_UserContext global_user_context;

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
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

#endif

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

#ifndef CONFIG_CAPDL_LOADER_VERIFIED

// From structures_gen.h (from the kernel).
// Probably should not be in here.
enum cap_tag {
    cap_null_cap = 0,
    cap_untyped_cap = 1,
    cap_endpoint_cap = 2,
    cap_async_endpoint_cap = 3,
    cap_reply_cap = 4,
    cap_cnode_cap = 5,
    cap_thread_cap = 6,
    cap_irq_control_cap = 7,
    cap_irq_handler_cap = 8,
    cap_zombie_cap = 9,
    cap_small_frame_cap = 10,
    cap_frame_cap = 11,
    cap_page_table_cap = 12,
    cap_page_directory_cap = 13,
    cap_asid_control_cap = 14,
    cap_asid_pool_cap = 15
};

char*
capType_to_string (int type) {
    switch (type) {
        case cap_null_cap:
            return "null_cap";
        case cap_untyped_cap:
            return "untyped_cap";
        case cap_endpoint_cap:
            return "endpoint_cap";
        case cap_async_endpoint_cap:
            return "async_endpoint_cap";
        case cap_reply_cap:
            return "reply_cap";
        case cap_cnode_cap:
            return "cnode_cap";
        case cap_thread_cap:
            return "thread_cap";
        case cap_irq_control_cap:
            return "irq_control_cap";
        case cap_irq_handler_cap:
            return "irq_handler_cap";
        case cap_zombie_cap:
            return "zombie_cap";
        case cap_small_frame_cap:
            return "small_frame_cap";
        case cap_frame_cap:
            return "frame_cap";
        case cap_page_table_cap:
            return "page_table_cap";
        case cap_page_directory_cap:
            return "page_directory_cap";
        case cap_asid_control_cap:
            return "asid_control_cap";
        case cap_asid_pool_cap:
            return "asid_pool_cap";
        default:
            return "Unknown cap type";
    }
}

#endif

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
*get_spec_object(const CDL_Model *spec, CDL_ObjID object_id)
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

/* elf file loading hack - prefill objects with the data defined in the elf file */
static seL4_CPtr
get_frame_cap(CDL_ObjID pd, uintptr_t vaddr, const CDL_Model *spec)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
    CDL_Cap *pt_cap = get_cap_at(cdl_pd, PD_SLOT(vaddr));
    if (pt_cap == NULL) {
        die("Could not find PT cap %s[%d]", CDL_Obj_Name(cdl_pd), (int)PD_SLOT(vaddr));
    }

    /* Check if the PT cap is actually a large frame cap. */
    if (pt_cap->type == CDL_FrameCap) {
	    return orig_caps(CDL_Cap_ObjID(pt_cap));
    }

    CDL_Object *cdl_pt = get_spec_object(spec, CDL_Cap_ObjID(pt_cap));
    CDL_Cap *frame_cap = get_cap_at(cdl_pt, PT_SLOT(vaddr));
    if (frame_cap == NULL) {
        die("Could not find frame cap %s[%d]", CDL_Obj_Name(cdl_pt), (int)PT_SLOT(vaddr));
    }

    return orig_caps(CDL_Cap_ObjID(frame_cap));
}

static seL4_CPtr
get_frame_size(CDL_ObjID pd, uintptr_t vaddr, const CDL_Model *spec)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
    CDL_Cap *pt_cap = get_cap_at(cdl_pd, PD_SLOT(vaddr));
    if (pt_cap == NULL) {
        die("Could not find PT cap %s[%d]", CDL_Obj_Name(cdl_pd), (int)PD_SLOT(vaddr));
    }

    /* Check if the PT cap is actually a large frame cap. */
    if (pt_cap->type == CDL_FrameCap) {
        return BIT(CDL_Obj_SizeBits(&spec->objects[CDL_Cap_ObjID(pt_cap)]));
    }

    CDL_Object *cdl_pt = get_spec_object(spec, CDL_Cap_ObjID(pt_cap));
    CDL_Cap *frame_cap = get_cap_at(cdl_pt, PT_SLOT(vaddr));
    if (frame_cap == NULL) {
        die("Could not find frame cap %s[%d]", CDL_Obj_Name(cdl_pt), (int)PT_SLOT(vaddr));
    }

    return BIT(CDL_Obj_SizeBits(&spec->objects[CDL_Cap_ObjID(frame_cap)]));
}

static seL4_CPtr
get_frame_pt(CDL_ObjID pd, uintptr_t vaddr, const CDL_Model *spec)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
    CDL_Cap *pt_cap = get_cap_at(cdl_pd, PD_SLOT(vaddr));
    if (pt_cap == NULL) {
        die("Could not find PT cap %s[%d]", CDL_Obj_Name(cdl_pd), (int)PD_SLOT(vaddr));
    }

    /* Check if the PT cap is actually a large frame cap. */
    if (pt_cap->type == CDL_FrameCap) {
	    return 0;
    }
    assert(orig_caps(CDL_Cap_ObjID(pt_cap)) != 0);

    return orig_caps(CDL_Cap_ObjID(pt_cap));
}

static seL4_ArchObjectType
seL4_frame_type(int size)
{
    switch (size) {
#ifdef CONFIG_ARCH_ARM
        case 12:
            return seL4_ARM_SmallPageObject;
        case 16:
            return seL4_ARM_LargePageObject;
        case 20:
            return seL4_ARM_SectionObject;
        case 24:
            return seL4_ARM_SuperSectionObject;
#elif defined(CONFIG_ARCH_IA32)
        case 12:
            return seL4_IA32_4K;
        case 22:
            return seL4_IA32_4M;
#endif
        default:
            die("illegal frame size");
    }
}

#ifndef CONFIG_CAPDL_LOADER_VERIFIED

void init_copy_frame(seL4_BootInfo *bootinfo)
{
    /* An original frame will be mapped, backing copy_addr_with_pt. For
     * correctness we should unmap this before mapping into this
     * address. We locate the frame cap by looking in boot info
     * and knowing that the userImageFrames are ordered by virtual
     * address in our address space. The flush is probably not
     * required, but doesn't hurt to be cautious.
     */
    seL4_CPtr copy_addr_frame = bootinfo->userImageFrames.start +
        ((uintptr_t)copy_addr_with_pt) / PAGE_SIZE_4K -
        ((uintptr_t)&__executable_start) / PAGE_SIZE_4K;
    seL4_CPtr copy_addr_pt = bootinfo->userImagePTs.start +
	PD_SLOT(((uintptr_t)copy_addr)) - PD_SLOT(((uintptr_t)&__executable_start));

    int error;

    for (int i = 0; i < sizeof(copy_addr_with_pt) / PAGE_SIZE_4K; i++) {
#ifdef ARCH_ARM
        error = seL4_ARM_Page_Unify_Instruction(copy_addr_frame + i, 0, PAGE_SIZE_4K);
        seL4_AssertSuccess(error);
#endif
        error = seL4_Page_Unmap(copy_addr_frame + i);
        seL4_AssertSuccess(error);

	if ((i + 1) % (1 << PT_SIZE) == 0) {
            error = seL4_PageTable_Unmap(copy_addr_pt + i / (1 << PT_SIZE));
            seL4_AssertSuccess(error);
	}
    }
}

static void
elf_load_frames(char *elf_name, CDL_ObjID pd, const CDL_Model *spec, seL4_BootInfo *bootinfo)
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
        debug_printf("    to %p... ", (void*)(uint32_t)elf_getProgramHeaderVaddr(elf_file, i));

        size_t f_len = elf_getProgramHeaderFileSize(elf_file, i);
        uintptr_t dest = elf_getProgramHeaderVaddr(elf_file, i);
        uintptr_t src = (uintptr_t) elf_file + elf_getProgramHeaderOffset(elf_file, i);

        //Skip non loadable headers
        if(elf_getProgramHeaderType(elf_file, i) != PT_LOAD) {
            debug_printf_("Skipping non loadable header\n");
            continue;
        }
        uintptr_t vaddr = dest;

        while (vaddr < dest + f_len) {
            debug_printf_(".");

            /* map frame into the loader's address space so we can write to it */
            seL4_CPtr sel4_page = get_frame_cap(pd, vaddr, spec);
            seL4_CPtr sel4_page_pt = get_frame_pt(pd, vaddr, spec);
            size_t sel4_page_size = get_frame_size(pd, vaddr, spec);

            int error = seL4_Page_Map(sel4_page, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                                      seL4_AllRights, seL4_Default_VMAttributes);
	    if (error == seL4_FailedLookup) {
                error = seL4_PageTable_Map(sel4_page_pt, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                                           seL4_Default_VMAttributes);
                seL4_AssertSuccess(error);
                error = seL4_Page_Map(sel4_page, seL4_CapInitThreadPD, (seL4_Word)copy_addr,
                                      seL4_AllRights, seL4_Default_VMAttributes);
	    }
            seL4_AssertSuccess(error);

            /* copy until end of section or end of page */
            size_t len = dest + f_len - vaddr;
            if (len > sel4_page_size - (vaddr % sel4_page_size)) {
                len = sel4_page_size - (vaddr % sel4_page_size);
            }
            memcpy((void *) (copy_addr + vaddr % sel4_page_size), (void *) (src + vaddr - dest), len);

#ifdef ARCH_ARM
            error = seL4_ARM_Page_Unify_Instruction(sel4_page, 0, sel4_page_size);
            seL4_AssertSuccess(error);
#endif
            error = seL4_Page_Unmap(sel4_page);
            seL4_AssertSuccess(error);

	    if (sel4_page_pt != 0) {
	        error = seL4_PageTable_Unmap(sel4_page_pt);
                seL4_AssertSuccess(error);
	    }

            vaddr += len;
        }

        /* This better be a 32-bit ELF for what we're about to do. */
        assert(((struct Elf32_Header*)elf_file)->e_ident[EI_CLASS] == ELFCLASS32);

        debug_printf_(" Marking header as loaded\n");
        /* Overwrite the section type so that next time this section is
         * encountered it will be skipped as it is not considered loadable. A
         * bit of a hack, but fine for now.
         */
        elf32_getProgramHeaderTable(elf_file)[i].p_type = PT_NULL;
    }
}
#endif //!CONFIG_CAPDL_LOADER_VERIFIED


/* --------------------------------------------------------------------------------------------- */


#ifndef CONFIG_CAPDL_LOADER_VERIFIED

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

    seL4_Word count[32] = {0};

    // Count how many untypeds there are of each size.
    for (seL4_Word untyped_index = 0; untyped_index != untyped_end - untyped_start; untyped_index++)
        count[bootinfo->untypedSizeBitsList[untyped_index]] += 1;

    // Calculate the starting index for each untyped.
    seL4_Word total = 0;
    for (seL4_Word size = 32; size != 0; size--) {
        seL4_Word oldCount = count[size];
        count[size] = total;
        total += oldCount;
    }

    // Store untypeds in untyped_cptrs array.
    for (seL4_Word untyped_index = 0; untyped_index != untyped_end - untyped_start; untyped_index++) {
        debug_printf("Untyped %3d (cptr=%x) is of size %2d. Placing in slot %d...\n",
                     untyped_index, untyped_start + untyped_index,
                     bootinfo->untypedSizeBitsList[untyped_index],
                     count[bootinfo->untypedSizeBitsList[untyped_index]]);

        untyped_cptrs[count[bootinfo->untypedSizeBitsList[untyped_index]]] = untyped_start +  untyped_index;
        count[bootinfo->untypedSizeBitsList[untyped_index]] += 1;
    }

}
#endif //!CONFIG_CAPDL_LOADER_VERIFIED

static void
parse_bootinfo(seL4_BootInfo *bootinfo)
{
    debug_printf("Parsing bootinfo...\n");

    free_slot_start = bootinfo->empty.start;
    free_slot_end = bootinfo->empty.end;

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
    /* When using libsel4platsupport for printing support, we end up using some
     * of our free slots during serial port initialisation. Skip over these to
     * avoid failing our own allocations. Note, this value is just hardcoded
     * for the amount of slots this initialisation currently uses up.
     * JIRA: CAMKES-204.
     */
    free_slot_start += 16;
#endif

    /* We need to be able to actual store caps to the maximum number of objects
     * we may be dealing with.
     * This check can still pass and initialisation fail as we need extra slots for duplicates
     * for CNodes and TCBs.
     */
    assert(free_slot_end - free_slot_start >= CONFIG_CAPDL_LOADER_MAX_OBJECTS);

    debug_printf("  %d free cap slots, from %d to %d\n", free_slot_end - free_slot_start, free_slot_start, free_slot_end);

#if CONFIG_CAPDL_LOADER_PRINT_UNTYPEDS
    /* Uncomment this section if you want to dump untyped regions for debugging
     * purposes.
     */
    int num_untyped = bootinfo->untyped.end - bootinfo->untyped.start;
    debug_printf("  Untyped memory (%d)\n", num_untyped);
    for (int i = 0; i < num_untyped; i++) {
        uintptr_t ut_paddr = bootinfo->untypedPaddrList[i];
        uintptr_t ut_size = bootinfo->untypedSizeBitsList[i];
        debug_printf("    0x%08x - 0x%08x\n", (unsigned int) ut_paddr,
                     (unsigned int) ut_paddr + (1 << ut_size));
    }
#endif

    debug_printf("Loader is running in domain %d\n", bootinfo->initThreadDomain);

#if CONFIG_CAPDL_LOADER_PRINT_DEVICE_INFO
    debug_printf("  Device memory (%d)\n", bootinfo->numDeviceRegions);
    for (unsigned int i = 0; i < bootinfo->numDeviceRegions; i++) {
        void *paddr = (void*)bootinfo->deviceRegions[i].basePaddr;
        int size = bootinfo->deviceRegions[i].frameSizeBits;
        int frames = bootinfo->deviceRegions[i].frames.end
        - bootinfo->deviceRegions[i].frames.start;
        debug_printf("    %p - %p (%d %d-bit frames)\n",
                     paddr, paddr + BIT(size) * frames, frames, size);
    }
#endif
}

#ifndef CONFIG_CAPDL_LOADER_VERIFIED

#ifdef CONFIG_KERNEL_STABLE
static int find_device_frame(void *paddr, seL4_CPtr free_slot, CDL_ObjID obj_id,
        seL4_BootInfo *bootinfo) {
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
    int error = simple_stable_get_frame_cap(bootinfo, paddr, seL4_PageBits, &path);
    seL4_AssertSuccess(error);
    add_sel4_cap(obj_id, ORIG, free_slot);
    return 0;
}
#else
static int find_device_frame(void *paddr, seL4_CPtr free_slot, CDL_ObjID obj_id,
        seL4_BootInfo *bootinfo) {
    for (unsigned int i = 0; i < bootinfo->numDeviceRegions; i++) {

        /* Search each of the frames that make up this region to see if
         * we can find one whose base matches the address we're looking
         * for.
         */
        for (unsigned int j = 0; j < bootinfo->deviceRegions[i].frames.end -
                bootinfo->deviceRegions[i].frames.start + 1; j++) {

            if (bootinfo->deviceRegions[i].basePaddr +
                    BIT(bootinfo->deviceRegions[i].frameSizeBits) * j == (seL4_Word)paddr) {
                /* We found it. */
                seL4_CPtr root = seL4_CapInitThreadCNode;
                int index = bootinfo->deviceRegions[i].frames.start + j;
                int depth = 32;

                int err = seL4_CNode_Copy(root, free_slot, depth,
                    root, index, depth, seL4_AllRights);
                seL4_AssertSuccess(err);
                add_sel4_cap(obj_id, ORIG, free_slot);
                return 0;
            }
        }
    }

    /* We failed to find this device frame. */
    return -1;
}
#endif

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
create_objects(const CDL_Model *spec, seL4_BootInfo *bootinfo)
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

#ifdef CONFIG_CAPDL_LOADER_VERIFIED
        seL4_CPtr untyped_cptr = bootinfo->untyped.start + ut_index;
#else
        seL4_CPtr untyped_cptr = untyped_cptrs[ut_index];
#endif

        CDL_Object *obj = &spec->objects[obj_id_index];

        CDL_ObjectType capdl_obj_type = CDL_Obj_Type(obj);
        seL4_ArchObjectType obj_type = seL4_ObjectTypeCount;
        int obj_size = 0;

#ifdef CONFIG_CAPDL_LOAD_PRINT_CAPDL_OBJECTS
        debug_printf("Creating object %s in slot %d, from untyped %x...\n", CDL_Obj_Name(obj), free_slot, untyped_cptr);
#endif

        // If object type is CDL_ASIDPool or CDL_Interrupt, don't make the object.
        // Later versions wanting basic CDL_ASIDPool support could make an
        /*
        if (obj_type == CDL_ASIDPool) {
            obj_type = CDL_Untyped;
            obj_size = 12; // ASID pools are 4K.
        }
         ... create object as usual ...
        if (CDL_Obj_Type(obj) == CDL_ASIDPool) {
            // We need to special case ASID creation because it needs something beyond Untyped_Retype.
            free_slot_index++;
            seL4_CPtr asid_slot = free_slot_start + free_slot_index;
            err = seL4_ASIDControl_MakePool(seL4_CapASIDControl, free_slot, seL4_CapInitThreadCNode, asid_slot, 32);
            seL4_AssertSuccess(err);
            free_slot = asid_slot;
        }
        */

#if !defined(CONFIG_CAPDL_LOADER_VERIFIED) && defined(ARCH_IA32)
        if (capdl_obj_type == CDL_IOPorts) {
            seL4_CPtr root = seL4_CapInitThreadCNode;
            int index = seL4_CapIOPort;
            int depth = 32;

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
            debug_printf(" device frame, paddr = %p\n", obj->paddr);

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
            /* This is a device frame. Look for it in bootinfo. */
            if (find_device_frame(obj->paddr, free_slot, obj_id, bootinfo) == 0) {
                /* We found and added the frame. */
                obj_id_index++;
                free_slot_index++;
                continue;
            }
#endif

            die("Failed to find device frame at paddr = %p\n", obj->paddr);
        }

        assert (capdl_obj_type != CDL_ASIDPool);

        // Never create Interrupt objects here
#if !defined(CONFIG_CAPDL_LOADER_VERIFIED) && defined(ARCH_IA32)
        if (capdl_obj_type == CDL_Interrupt || capdl_obj_type == CDL_IOPorts || capdl_obj_type == CDL_IODevice) {
#else
        if (capdl_obj_type == CDL_Interrupt) {
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
            } else {
                obj_type = (seL4_ArchObjectType) capdl_obj_type;
            }
        }
        
        // Create object
#if !defined(CONFIG_CAPDL_LOADER_VERIFIED) && defined(ARCH_IA32)
        if (capdl_obj_type != CDL_Interrupt && capdl_obj_type != CDL_IOPorts && capdl_obj_type != CDL_IODevice) {
#else
        if (capdl_obj_type != CDL_Interrupt) {
#endif
            int err = retype_untyped(free_slot, untyped_cptr, obj_type, obj_size);

            if (err == seL4_NoError) {
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
create_irq_cap(CDL_IRQ irq, seL4_CPtr free_slot)
{
    seL4_CPtr root = seL4_CapInitThreadCNode;
    int index = free_slot;
    int depth = 32;

    int error = seL4_IRQControl_Get(seL4_CapIRQControl, irq, root, index, depth);
    seL4_AssertSuccess(error);

    add_sel4_cap(irq, IRQ, index);
}

static void
create_irq_caps(const CDL_Model *spec)
{
    debug_printf("Creating irq handler caps...\n");

    for (CDL_IRQ irq = 0; irq < CONFIG_CAPDL_LOADER_MAX_IRQS; irq++) {
        if (spec->irqs[irq] != INVALID_OBJ_ID) {
            seL4_CPtr free_slot = get_free_slot();

            debug_printf(" Creating irq handler cap for IRQ %d...\n", irq);
            create_irq_cap(irq, free_slot);
            next_free_slot();
        }
    }
}

/* Duplicate capabilities */
static void
duplicate_cap(CDL_ObjID object_id, int free_slot)
{
    seL4_CapRights rights = seL4_AllRights;

    seL4_CPtr dest_root = seL4_CapInitThreadCNode;
    int dest_index = free_slot;
    int dest_depth = 32;

    seL4_CPtr src_root = seL4_CapInitThreadCNode;
    int src_index = orig_caps(object_id);
    int src_depth = 32;

//    debug_printf(" Free slot %d...\n", free_slot);
//    debug_printf(" dest_index %d...\n", dest_index);
//    debug_printf(" src_index %d...\n", src_index);

    int error = seL4_CNode_Copy(dest_root, dest_index, dest_depth,
                                src_root, src_index, src_depth, rights);
    seL4_AssertSuccess(error);

    add_sel4_cap(object_id, DUP, dest_index);
}

static void
duplicate_caps(const CDL_Model *spec)
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

/* Initialise TCBs */
static void
init_tcb(const CDL_Model *spec, CDL_ObjID tcb)
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
    CDL_Cap *cdl_fault_ep    = get_cap_at(cdl_tcb, CDL_TCB_FaultEP_Slot);
    if (cdl_fault_ep == NULL) {
        debug_printf("  Warning: TCB has no fault endpoint\n");
    }
    CDL_Cap *cdl_ipcbuffer   = get_cap_at(cdl_tcb, CDL_TCB_IPCBuffer_Slot);
    if (cdl_ipcbuffer == NULL) {
        debug_printf("  Warning: TCB has no IPC buffer\n");
    }

    seL4_Word ipcbuffer_addr = CDL_TCB_IPCBuffer_Addr(cdl_tcb);
    uint8_t priority = CDL_TCB_Priority(cdl_tcb);

    seL4_CPtr sel4_tcb = orig_caps(tcb);

    seL4_CPtr sel4_cspace_root = cdl_cspace_root == NULL ? 0 : orig_caps(CDL_Cap_ObjID(cdl_cspace_root));
    seL4_CPtr sel4_vspace_root = orig_caps(CDL_Cap_ObjID(cdl_vspace_root));
    seL4_CPtr sel4_ipcbuffer   = cdl_ipcbuffer ? orig_caps(CDL_Cap_ObjID(cdl_ipcbuffer)) : 0;
    seL4_CPtr sel4_fault_ep    = cdl_fault_ep ? orig_caps(CDL_Cap_ObjID(cdl_fault_ep)) : 0;

    seL4_CapData_t sel4_cspace_root_data = cdl_cspace_root == NULL ? (seL4_CapData_t){{0}} : get_capData(CDL_Cap_Data(cdl_cspace_root));
    seL4_CapData_t sel4_vspace_root_data = get_capData(CDL_Cap_Data(cdl_vspace_root));

    int error = seL4_TCB_Configure(sel4_tcb, sel4_fault_ep, priority,
                                   sel4_cspace_root, sel4_cspace_root_data,
                                   sel4_vspace_root, sel4_vspace_root_data,
                                   ipcbuffer_addr, sel4_ipcbuffer);

    seL4_AssertSuccess(error);
}

static void
configure_thread(const CDL_Model *spec, CDL_ObjID tcb)
{
    seL4_CPtr sel4_tcb = dup_caps(tcb);

    CDL_Object *cdl_tcb = get_spec_object(spec, tcb);
    const seL4_Word *argv = cdl_tcb->tcb_extra.init;
    int argc = cdl_tcb->tcb_extra.init_sz;

    uintptr_t sp = CDL_TCB_SP(cdl_tcb);
    uintptr_t pc = CDL_TCB_PC(cdl_tcb);

    if (sp % (sizeof(uintptr_t) * 2) != 0) {
        die("TCB %s's stack pointer is not dword-aligned", CDL_Obj_Name(&spec->objects[tcb]));
    }
    int reg_args = 0;
#if defined(ARCH_ARM)
    /* On ARM, the first four arguments go in registers. */
    reg_args = 4;
#endif
#if defined(ARCH_IA32) && defined(CONFIG_CAPDL_LOADER_CC_REGISTERS)
    reg_args = 4;
#endif

#ifdef CONFIG_CAPDL_LOADER_VERIFIED
    assert(argc <= reg_args);
#else
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

        /* Ensure that the final stack pointer will be dword-aligned. */
        if ((argc - reg_args) % 2 == 1 && argc - reg_args > 0) {
            sp -= sizeof(uintptr_t);
        }

        /* Find and map the frame representing the TCB's stack. Note that we do
         * `sp - sizeof(uintptr_t)` because the stack pointer may be on a page
         * boundary.
         */
        seL4_CPtr frame = get_frame_cap(pd, sp - sizeof(uintptr_t), spec);
        /* FIXME: The above could actually fail messily if the user has given a
         * spec with stack pointers that point outside the ELF image.
         */
        int error = seL4_Page_Map(frame, seL4_CapInitThreadPD, (seL4_Word)copy_addr_with_pt,
                                  seL4_AllRights, seL4_Default_VMAttributes);
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
            sp -= sizeof(uintptr_t);
            *(uintptr_t*)(copy_addr_with_pt + sp % PAGE_SIZE_4K) = argv[i];
        }

#ifdef ARCH_ARM
        error = seL4_ARM_Page_Unify_Instruction(frame, 0, PAGE_SIZE_4K);
        seL4_AssertSuccess(error);
#endif //ARCH_ARM
        error = seL4_Page_Unmap(frame);
        seL4_AssertSuccess(error);
#endif //CONFIG_CAPDL_LOADER_CC_REGISTERS
    }
#endif //!CONFIG_CAPDL_LOADER_VERIFIED

    seL4_UserContext regs = {
#if defined(ARCH_ARM)
        .pc = pc,
        .sp = sp,
        .r0 = argc > 0 ? argv[0] : 0,
        .r1 = argc > 1 ? argv[1] : 0,
        .r2 = argc > 2 ? argv[2] : 0,
        .r3 = argc > 3 ? argv[3] : 0,
#elif defined(ARCH_IA32)
        .eip = pc,
        .esp = sp,
#ifdef CONFIG_CAPDL_LOADER_CC_REGISTERS
        .eax = argc > 0 ? argv[0] : 0,
        .ebx = argc > 1 ? argv[1] : 0,
        .ecx = argc > 2 ? argv[2] : 0,
        .edx = argc > 3 ? argv[3] : 0,
#endif
#endif
    };
    debug_printf("  Setting up _start(");
    for (int i = 0; i < argc; i++) {
        debug_printf_("%p", (void*)argv[i]);
        if (i != argc - 1) {
            debug_printf_(", ");
        }
    }
    debug_printf_(")...\n");
    debug_printf("pc = %p\n", (void*)pc);
    debug_printf("sp = %p\n", (void*)sp);

    global_user_context = regs;

    int error = seL4_TCB_WriteRegisters(sel4_tcb, false, 0,
                                        sizeof(seL4_UserContext) / sizeof(seL4_Word),
                                        &global_user_context);
    seL4_AssertSuccess(error);

    uint32_t domain = CDL_TCB_Domain(cdl_tcb);
    debug_printf("  Assigning thread to domain %u...\n", domain);
    error = seL4_DomainSet_Set(seL4_CapDomain, domain, sel4_tcb);
    seL4_AssertSuccess(error);
}

static void
init_tcbs(const CDL_Model *spec)
{
    debug_printf("Initialising TCBs...\n");
    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_TCB) {
            debug_printf(" Initialising %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            init_tcb(spec, obj_id);

            debug_printf(" Configuring %s...\n", CDL_Obj_Name(&spec->objects[obj_id]));
            configure_thread(spec, obj_id);
        }
    }
}

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
static void
init_elf(const CDL_Model *spec, CDL_ObjID tcb, seL4_BootInfo *bootinfo)
{
    CDL_Object *cdl_tcb = get_spec_object(spec, tcb);

    CDL_Cap *cdl_vspace_root = get_cap_at(cdl_tcb, CDL_TCB_VTable_Slot);
    if (cdl_vspace_root == NULL) {
        die("Could not find VSpace cap for %s", CDL_Obj_Name(cdl_tcb));
    }

    elf_load_frames(CDL_TCB_ElfName(cdl_tcb), CDL_Cap_ObjID(cdl_vspace_root), spec, bootinfo);
}

static void
init_elfs(const CDL_Model *spec, seL4_BootInfo *bootinfo)
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
#endif //!CONFIG_CAPDL_LOADER_VERIFIED

static void
init_irq(const CDL_Model *spec, CDL_IRQ irq_no)
{
    seL4_CPtr irq_handler_cap = irq_caps(irq_no);

    CDL_Object *cdl_irq = get_spec_object(spec, spec->irqs[irq_no]);
    assert(cdl_irq->type == CDL_Interrupt);
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

        int error = seL4_IRQHandler_SetEndpoint (irq_handler_cap, endpoint_cptr);
        seL4_AssertSuccess(error);
    }
}

static void
init_irqs(const CDL_Model *spec)
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
set_asid(CDL_ObjID page)
{
    debug_printfn("(%s)\n", CDL_Obj_Name(&spec->objects[page]));

    seL4_CPtr sel4_page = orig_caps(page);
    int error = seL4_ASIDPool_Assign(seL4_CapInitThreadASIDPool, sel4_page);
    seL4_AssertSuccess(error);
}
#endif //!CONFIG_KERNEL_STABLE

static void
init_pd_asid(const CDL_Model *spec UNUSED, CDL_ObjID pd)
{
    debug_printfn("(%s)\n", CDL_Obj_Name(&spec->objects[pd]));
#ifndef CONFIG_KERNEL_STABLE
    set_asid(pd);
#endif //!CONFIG_KERNEL_STABLE
}

static void
init_pd_asids(const CDL_Model *spec)
{
    debug_printf("Initialising Page Directory ASIDs...\n");

    for (CDL_ObjID obj_id = 0; obj_id < spec->num; obj_id++) {
        if (spec->objects[obj_id].type == CDL_PD) {
            debug_printf(" Initialising page directory ASID %s...\n",
                         CDL_Obj_Name(&spec->objects[obj_id]));
            init_pd_asid(spec, obj_id);
        }
    }    
}

static void
map_page(CDL_Cap *page_cap, CDL_ObjID pd_id,
         seL4_CapRights rights, seL4_Word page_vaddr, seL4_VMAttributes vm_attribs)
{
    CDL_ObjID page = CDL_Cap_ObjID(page_cap);
    debug_printfn("(%s, %s, rights=%x, vaddr=%x, vm_attribs=%x)\n",
                  CDL_Obj_Name(&spec->objects[page]),
                  CDL_Obj_Name(&spec->objects[pd_id]),
                  rights, page_vaddr, vm_attribs);

    // TODO: We should not be using the original cap here
    seL4_CPtr sel4_page = orig_caps(page);
    seL4_CPtr sel4_pd = orig_caps(pd_id);

    if (CDL_Cap_Type(page_cap) == CDL_PTCap) {
        int error = seL4_PageTable_Map(sel4_page, sel4_pd, page_vaddr, vm_attribs);
        seL4_AssertSuccess(error);

    } else if (CDL_Cap_Type(page_cap) == CDL_FrameCap) {
#ifdef CAPDL_SHARED_FRAMES
        /* hack to support shared frames: create a new cap for each mapping */
        int dest_index = get_free_slot();

        int error_0 = seL4_CNode_Copy(seL4_CapInitThreadCNode, dest_index, 32,
                                      seL4_CapInitThreadCNode, sel4_page, 32, seL4_AllRights);
        seL4_AssertSuccess(error_0);

        next_free_slot();
        sel4_page = dest_index;
#endif

        /* XXX: Write-only mappings are silently downgraded by the kernel to
         * kernel-only. This is clearly not what the user intended if they
         * passed us a write-only mapping. Help them out by upgrading it here.
         */
        if (rights & seL4_CanWrite) {
            rights |= seL4_CanRead;
        }

        // FIXME: Add support for super-pages.
        int error = seL4_Page_Map(sel4_page, sel4_pd, page_vaddr, rights, vm_attribs);
        seL4_AssertSuccess(error);
    }
}

static void
map_page_directory_slot(const CDL_Model *spec UNUSED, CDL_ObjID pd, CDL_CapSlot *pd_slot)
{
    debug_printfn("(%s, %d)\n", CDL_Obj_Name(&spec->objects[pd]), pd_slot->slot);
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pd_slot);

    seL4_Word page_vaddr = CDL_CapSlot_Slot(pd_slot) << (PT_SIZE + FRAME_SIZE);
    seL4_CapRights page_rights = CDL_Cap_Rights(page_cap);
    seL4_VMAttributes vm_attribs = CDL_Cap_VMAttributes(page_cap);

    map_page(page_cap, pd, page_rights, page_vaddr, vm_attribs);
}

static void
map_page_directory(const CDL_Model *spec, CDL_ObjID pd)
{
    debug_printfn("(%s)\n", CDL_Obj_Name(&spec->objects[pd]));

    CDL_Object *cdl_pd = get_spec_object(spec, pd);

    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++)
        map_page_directory_slot(spec, pd, CDL_Obj_GetSlot(cdl_pd, slot_index));
}

static void
map_page_table_slot(const CDL_Model *spec UNUSED, CDL_ObjID pd, CDL_ObjID pt UNUSED,
                    seL4_Word pt_vaddr, CDL_CapSlot *pt_slot)
{
    debug_printfn("(%s, %s, %x, %d)\n", CDL_Obj_Name(&spec->objects[pd]),
                                        CDL_Obj_Name(&spec->objects[pt]),
                                        pt_vaddr, pt_slot->slot);
    CDL_Cap *page_cap = CDL_CapSlot_Cap(pt_slot);

    seL4_Word page_vaddr = pt_vaddr + (CDL_CapSlot_Slot(pt_slot) << FRAME_SIZE);
    seL4_CapRights page_rights = CDL_Cap_Rights(page_cap);
    seL4_VMAttributes vm_attribs = CDL_Cap_VMAttributes(page_cap);

    map_page(page_cap, pd, page_rights, page_vaddr, vm_attribs);
}

static void
map_page_table_slots(const CDL_Model *spec, CDL_ObjID pd, CDL_CapSlot *pd_slot)
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
map_page_directory_page_tables(const CDL_Model *spec, CDL_ObjID pd)
{
    CDL_Object *cdl_pd = get_spec_object(spec, pd);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_pd); slot_index++)
        map_page_table_slots(spec, pd, CDL_Obj_GetSlot(cdl_pd, slot_index));
}

static void
init_vspace(const CDL_Model *spec)
{
    debug_printf("Initialising VSpaces...\n");

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
}

/* Initialise capability spaces */
static void
init_cnode_slot(const CDL_Model *spec, init_cnode_mode mode, CDL_ObjID cnode_id, CDL_CapSlot *cnode_slot)
{
    CDL_Cap *target_cap = CDL_CapSlot_Cap(cnode_slot);
    CDL_ObjID target_cap_obj = CDL_Cap_ObjID(target_cap);
    CDL_IRQ target_cap_irq = CDL_Cap_IRQ(target_cap);

    CDL_CapType target_cap_type = CDL_Cap_Type(target_cap);
    seL4_CapRights target_cap_rights = CDL_Cap_Rights(target_cap);

    // For endpoint this is the badge, for cnodes, this is the (encoded) guard.
    seL4_CapData_t target_cap_data = get_capData(CDL_Cap_Data(target_cap));

    /* To support moving original caps, we need a spec with a CDT (most don't).
     * This shoud probably become a separate configuration option for when to
     * use the CDT, and when to just copy. For now, let's just copy.
     */
#if defined(CONFIG_CAPDL_LOADER_VERIFIED)
    int is_orig_cap = CDL_Cap_IsOrig(target_cap);
#else
    int is_orig_cap = false; //FIXME
#endif
    int is_ep_cap = (target_cap_type == CDL_EPCap || target_cap_type == CDL_AEPCap);
    int is_irq_handler_cap = (target_cap_type == CDL_IRQHandlerCap);
#if !defined(CONFIG_CAPDL_LOADER_VERIFIED) && defined(ARCH_IA32)
    int is_ioport_cap = (target_cap_type == CDL_IOPortsCap);
    int is_iospace_cap = (target_cap_type == CDL_IOSpaceCap);
#endif

    CDL_Object *dest_obj = get_spec_object(spec, cnode_id);
    uint8_t dest_size = CDL_Obj_SizeBits(dest_obj);

    // Use a copy of the cap to reference the destination, in case the original has already been moved.
    seL4_CPtr dest_root = dup_caps(cnode_id);
    int dest_index = CDL_CapSlot_Slot(cnode_slot);
    uint8_t dest_depth = dest_size;

    // Use an original cap to reference the object to copy.
    seL4_CPtr src_root = seL4_CapInitThreadCNode;
#if !defined(CONFIG_CAPDL_LOADER_VERIFIED) && defined(ARCH_IA32)
    int src_index;
    if (is_ioport_cap) {
	    src_index = seL4_CapIOPort;
	} else if (is_iospace_cap) {
	    src_index = seL4_CapIOSpace;
    } else if (is_irq_handler_cap) {
	    src_index = irq_caps(target_cap_irq);
    } else {
	    src_index = orig_caps(target_cap_obj);
    }
#else
    int src_index = is_irq_handler_cap ? irq_caps(target_cap_irq) : orig_caps(target_cap_obj);
#endif

    uint8_t src_depth = 32;

    if ((mode == MOVE) && is_orig_cap) {
        if (is_ep_cap || is_irq_handler_cap) {
            debug_printf_("moving...\n");
            int error = seL4_CNode_Move(dest_root, dest_index, dest_depth,
                                        src_root, src_index, src_depth);
            seL4_AssertSuccess(error);
        } else {
            debug_printf_("mutating (with badge/guard %p)...\n", (void*)target_cap_data.words[0]);
            int error = seL4_CNode_Mutate(dest_root, dest_index, dest_depth,
                                          src_root, src_index, src_depth, target_cap_data);
            seL4_AssertSuccess(error);
        }
    } else if ((mode == COPY) && !is_orig_cap) {
        debug_printf_("minting (with badge/guard %p)...\n", (void*)target_cap_data.words[0]);
        int error = seL4_CNode_Mint(dest_root, dest_index, dest_depth,
                                    src_root, src_index, src_depth, target_cap_rights, target_cap_data);
        seL4_AssertSuccess(error);
    } else {
        debug_printf_("skiping\n");
    }
}

static void
init_cnode(const CDL_Model *spec, init_cnode_mode mode, CDL_ObjID cnode)
{
    CDL_Object *cdl_cnode = get_spec_object(spec, cnode);
    for (unsigned int slot_index = 0; slot_index < CDL_Obj_NumSlots(cdl_cnode); slot_index++) {
        if (CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.type == CDL_IRQHandlerCap) {
            CDL_IRQ irq = CDL_Obj_GetSlot(cdl_cnode, slot_index)->cap.irq;
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
init_cspace(const CDL_Model *spec)
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
start_threads(const CDL_Model *spec)
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
init_system(const CDL_Model *spec)
{
    seL4_BootInfo *bootinfo = seL4_GetBootInfo();

#ifndef CONFIG_CAPDL_LOADER_VERIFIED
    init_copy_frame(bootinfo);
#endif

    parse_bootinfo(bootinfo);
#ifndef CONFIG_CAPDL_LOADER_VERIFIED
    sort_untypeds(bootinfo);
#endif

    create_objects(spec, bootinfo);
    create_irq_caps(spec);
    duplicate_caps(spec);

    init_irqs(spec);
    init_pd_asids(spec);
#ifndef CONFIG_CAPDL_LOADER_VERIFIED
    init_elfs(spec, bootinfo);
#endif
    init_vspace(spec);
    init_tcbs(spec);
    init_cspace(spec);
    start_threads(spec);
}

int
main(void)
{
    SET_MUSLC_SYSCALL_TABLE;
#ifndef CONFIG_CAPDL_LOADER_VERIFIED
    /* Allow us to print via seL4_Debug_PutChar. */
    platsupport_serial_setup_bootinfo_failsafe();
#endif

    debug_printf("Starting Loader...\n");
    init_system(&capdl_spec);

    debug_printf("We used %d CSlots (%.2LF%% of our CNode)\n", get_free_slot(),
        (long double)get_free_slot() /
        (long double)(1U << CONFIG_ROOT_CNODE_SIZE_BITS) * 100);
    debug_printf(ANSI_GREEN "Done; suspending..." ANSI_RESET "\n");
    seL4_TCB_Suspend(seL4_CapInitThreadTCB);
}
