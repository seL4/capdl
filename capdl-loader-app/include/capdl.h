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

#ifndef CAPDL_H__
#define CAPDL_H__

#include <autoconf.h>
#include <sel4/types.h>
#include <sel4utils/mapping.h>
#include <autoconf.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <utils/util.h>

#define CDL_VM_CacheEnabled         seL4_ARCH_Default_VMAttributes
#define CDL_VM_CacheDisabled        seL4_ARCH_Uncached_VMAttributes

#if defined(CONFIG_ARCH_ARM)

/* ARM does not support write through */
#define CDL_VM_WriteThrough         CDL_VM_CacheDisabled
/* Note that this is the number of bits translated by the PT
 * not the size of the actual PT object */
#elif defined(CONFIG_ARCH_X86)

#define CDL_VM_WriteThrough         seL4_X86_WriteThrough

#endif

/* Binary CapDL representation -- capdl.h */

/* CapRights:  Access rights of capabilities
 * This type is used internally by capDL, and doesn't necessarily
 * reflect the representation of cap rights in the kernel api.
 */
typedef enum {
    CDL_CanWrite = BIT(0),
    CDL_CanRead = BIT(1),
    CDL_CanGrant = BIT(2),
    CDL_AllRights = (BIT(0) | BIT(1) | BIT(2)),
} CDL_CapRights;

/* ObjectID: index into the objects array */
typedef seL4_Word CDL_ObjID;
#define INVALID_OBJ_ID ((CDL_ObjID)-1)

/* IRQ number: Hardware IRQ number */
typedef seL4_Word CDL_IRQ;
/* Core id */
typedef seL4_Word CDL_Core;

/* Capability: */
typedef enum {
    CDL_NullCap,
    CDL_UntypedCap,
    CDL_EPCap,
    CDL_NotificationCap,
    CDL_ReplyCap,
    CDL_MasterReplyCap,
    CDL_CNodeCap,
    CDL_TCBCap,
    CDL_IRQControlCap,
    CDL_IRQHandlerCap,
    CDL_FrameCap,
    CDL_PTCap,
    CDL_PDCap,
    CDL_PML4Cap,
    CDL_PDPTCap,
    CDL_ASIDControlCap,
    CDL_ASIDPoolCap,
#if defined(CONFIG_ARCH_X86)
    CDL_IOPortsCap,
    CDL_IOSpaceCap,
#endif
#if defined(CONFIG_ARCH_ARM)
    CDL_ARMIOSpaceCap,
#endif
    CDL_SCCap,
    CDL_SchedControlCap,
    CDL_RTReplyCap,
} CDL_CapType;

#define CDL_CapData_Badge 0
#define CDL_CapData_Guard 1
#define CDL_CapData_Raw   2

typedef struct {
    union {
        struct {
            seL4_Word guard_bits: 18; /* guards have an 18-bit value */
            seL4_Word guard_size: seL4_WordBits - 18;
        };
        seL4_Word badge;
        seL4_Word data;
    };
    unsigned tag: 2; /* One of CDL_CapData_* */
} PACKED CDL_CapData;

typedef struct {
    CDL_ObjID obj_id;
    CDL_CapData data;
    CDL_IRQ irq;
    CDL_ObjID mapping_container_id;
    seL4_Word mapping_slot;
    seL4_CPtr mapped_frame_cap;

    /* This type tag actually has a more specific type, but we force it to be represented as a
     * uint8_t to compress the size of this struct in memory.
     */
    /* CDL_CapType */ uint8_t type;

    /* The following map to more specific seL4 types, but the seL4 types are padded to word size,
     * wasting space. This padding is necessary for ABI compatibility, but we have no such
     * requirements here and can instead reduce the in-memory size of specs by packing these fields
     * into fewer bits.
     */
    /* seL4_ARCH_VMAttributes */ unsigned vm_attribs: 3;
    /* bool                   */ unsigned is_orig: 1;
    /* seL4_CapRights         */ unsigned rights: 3;
} PACKED CDL_Cap;

/* CapMap: is just an array of cap slots, position of the slot and cap */
typedef struct {
    seL4_Word slot;
    CDL_Cap cap;
} PACKED CDL_CapSlot;

typedef struct {
    seL4_Word num;
    CDL_CapSlot *slot;
} CDL_CapMap;

/* ObjMap: is just an array of object slots */
typedef struct {
    seL4_Word slot;
    CDL_ObjID id;
} CDL_ObjSlot;

typedef struct {
    seL4_Word num;
    CDL_ObjSlot *slot;
} CDL_ObjMap;

/* KernelObject: */
typedef enum {
    CDL_Endpoint      = seL4_EndpointObject,
    CDL_Notification  = seL4_NotificationObject,
    CDL_TCB           = seL4_TCBObject,
    CDL_CNode         = seL4_CapTableObject,
    CDL_Untyped       = seL4_UntypedObject,
#if defined(CONFIG_ARCH_ARM)
    CDL_PT            = seL4_ARM_PageTableObject,
    CDL_PD            = seL4_ARM_PageDirectoryObject,
    CDL_Frame         = seL4_ARM_SmallPageObject,
#elif defined(CONFIG_ARCH_X86)
    CDL_PT            = seL4_X86_PageTableObject,
    CDL_PD            = seL4_X86_PageDirectoryObject,
    CDL_Frame         = seL4_X86_4K,
#ifdef CONFIG_ARCH_X86_64
    CDL_PML4          = seL4_X64_PML4Object,
    CDL_PDPT          = seL4_X86_PDPTObject,
#endif
#endif
    CDL_ASIDPool      = seL4_ObjectTypeCount + 1,
    CDL_Interrupt     = seL4_ObjectTypeCount + 2,
#if defined(CONFIG_ARCH_X86)
    CDL_IOPorts       = seL4_ObjectTypeCount + 3,
    CDL_IODevice      = seL4_ObjectTypeCount + 4,
#endif
#ifdef CONFIG_KERNEL_RT
    CDL_SchedContext  = seL4_SchedContextObject,
    CDL_RTReply  = seL4_ReplyObject,
#else
    CDL_SchedContext  = seL4_ObjectTypeCount + 5,
    CDL_RTReply  = seL4_ObjectTypeCount + 6,
#endif
#if defined(CONFIG_ARCH_X86)
    CDL_IOAPICInterrupt = seL4_ObjectTypeCount + 7,
    CDL_MSIInterrupt = seL4_ObjectTypeCount + 8,
#endif
#if defined(CONFIG_ARCH_ARM)
    CDL_ARMIODevice   = seL4_ObjectTypeCount + 9,
#endif
} CDL_ObjectType;

typedef struct {
    uint8_t priority;
    uint8_t max_priority;
    uint8_t affinity;
    seL4_Word pc;
    seL4_Word sp;
    const char *elf_name;
    const seL4_Word *init;
    seL4_Word init_sz;
    seL4_CPtr fault_ep;
    /* The IPC buffer is at least 512-byte aligned, so we can drop the low 9 bits. */
    seL4_Word ipcbuffer_addr_upper_bits: seL4_WordBits - 9;
    uint8_t domain;
} CDL_TCBExtraInfo;

typedef struct {
    uint64_t period;
    uint64_t budget;
    seL4_Word data;
} CDL_SCExtraInfo;

typedef struct {
    int ioapic;
    int ioapic_pin;
    int level;
    int polarity;
} CDL_IOAPICIRQExtraInfo;

typedef struct {
    int handle;
    int pci_bus;
    int pci_dev;
    int pci_fun;
} CDL_MSIIRQExtraInfo;

typedef struct {
#ifdef CONFIG_CAPDL_LOADER_PRINTF
    const char *name; /* textual ObjID from the capDL spec */
#endif

    CDL_CapMap slots;
    union {
        CDL_TCBExtraInfo tcb_extra;
        CDL_SCExtraInfo sc_extra;
        CDL_IOAPICIRQExtraInfo ioapicirq_extra;
        CDL_MSIIRQExtraInfo msiirq_extra;
        void *paddr; /* Physical address; only relevant for frames and untyped objects. */
        CDL_Core core; /* core for sched control */
        struct {
            seL4_Word start;
            seL4_Word end;
        };
    };
    /* The following member has a more specific type, CDL_ObjectType, but by forcing it into a
     * uint8_t we can reduce the size of this struct.
     */
    /* CDL_ObjectType */ uint8_t type;
    /* The configuration for IO ports on x86 is currently overloaded into the
     * size_bits parameter. */
    uint32_t size_bits;

} PACKED CDL_Object;

typedef struct {
    CDL_ObjID frame;
    const char *type;
    size_t dest_offset;
    char *extra_information;
} PACKED CDL_FrameFill;

/* CapDLModel: is described by a map from ObjectIDs (array index) to Objects */
typedef struct {

    seL4_Word num;
    CDL_Object *objects;
    seL4_Word num_frame_fill;
    CDL_FrameFill *frame_fill;

    CDL_ObjID irqs[CONFIG_CAPDL_LOADER_MAX_IRQS];
} CDL_Model;

/* helper functions ---------------------------------------------------------------------------- */

#define CDL_TCB_CTable_Slot         0
#define CDL_TCB_VTable_Slot         1
#define CDL_TCB_Reply_Slot          2
#define CDL_TCB_Caller_Slot         3
#define CDL_TCB_IPCBuffer_Slot      4
#define CDL_TCB_FaultEP_Slot        5
#define CDL_TCB_SC_Slot             6
#define CDL_TCB_TemporalFaultEP_Slot   7

#define CDL_CapData_MakeGuard(x, y) \
{ .tag = CDL_CapData_Guard, .guard_bits = (y), .guard_size = (x) }

static inline CDL_CapType    CDL_Cap_Type(CDL_Cap *cap)
{
    return cap->type;
}
static inline CDL_CapData    CDL_Cap_Data(CDL_Cap *cap)
{
    return cap->data;
}
static inline seL4_ARCH_VMAttributes CDL_Cap_VMAttributes(CDL_Cap *cap)
{
    return cap->vm_attribs;
}
static inline CDL_ObjID      CDL_Cap_ObjID(CDL_Cap *cap)
{
    return cap->obj_id;
}
static inline CDL_CapRights  CDL_Cap_Rights(CDL_Cap *cap)
{
    return cap->rights;
}
static inline CDL_IRQ        CDL_Cap_IRQ(CDL_Cap *cap)
{
    return cap->irq;
}
static inline bool           CDL_Cap_IsOrig(CDL_Cap *cap)
{
    return cap->is_orig;
}

static inline seL4_Word      CDL_CapSlot_Slot(CDL_CapSlot *cap_slot)
{
    return cap_slot->slot;
}
static inline CDL_Cap *      CDL_CapSlot_Cap(CDL_CapSlot *cap_slot)
{
    return &cap_slot->cap;
}

static inline seL4_Word      CDL_ObjSlot_Slot(CDL_ObjSlot *obj_slot)
{
    return obj_slot->slot;
}
static inline CDL_ObjID      CDL_ObjSlot_ObjID(CDL_ObjSlot *obj_slot)
{
    return obj_slot->id;
}

/* Returns the sel4utils representation of a CDL_Cap's rights */
static inline seL4_CapRights_t CDL_seL4_Cap_Rights(CDL_Cap *cap)
{
    return seL4_CapRights_new(!!(cap->rights & CDL_CanGrant),
                              !!(cap->rights & CDL_CanRead),
                              !!(cap->rights & CDL_CanWrite));
}

static inline const char *CDL_Obj_Name(CDL_Object *obj)
{
#ifdef CONFIG_CAPDL_LOADER_PRINTF
    if (obj->name == NULL) {
        return "<unnamed>";
    } else {
        return obj->name;
    }
#else
    return "<unnamed>";
#endif
}

static inline CDL_ObjectType CDL_Obj_Type(CDL_Object *obj)
{
    return obj->type;
}
static inline seL4_Word      CDL_Obj_SizeBits(CDL_Object *obj)
{
    return obj->size_bits;
}
static inline seL4_Word      CDL_Obj_NumSlots(CDL_Object *obj)
{
    return obj->slots.num;
}
static inline CDL_CapSlot *
CDL_Obj_GetSlot(CDL_Object *obj, seL4_Word i)
{
    return &obj->slots.slot[i];
}

static inline seL4_Word
CDL_TCB_IPCBuffer_Addr(CDL_Object *obj)
{
    return ((seL4_Word)obj->tcb_extra.ipcbuffer_addr_upper_bits) << 9;
}

static inline uint8_t
CDL_TCB_Priority(CDL_Object *obj)
{
    return obj->tcb_extra.priority;
}

static inline uint8_t
CDL_TCB_MaxPriority(CDL_Object *obj)
{
    return obj->tcb_extra.max_priority;
}

static inline uint8_t
CDL_TCB_Affinity(CDL_Object *obj)
{
    return obj->tcb_extra.affinity;
}

static inline uint32_t
CDL_TCB_Domain(CDL_Object *obj)
{
    return obj->tcb_extra.domain;
}

static inline seL4_Word
CDL_TCB_PC(CDL_Object *obj)
{
    return obj->tcb_extra.pc;
}

static inline seL4_Word
CDL_TCB_SP(CDL_Object *obj)
{
    return obj->tcb_extra.sp;
}

static inline const char *
CDL_TCB_ElfName(CDL_Object *obj)
{
    return obj->tcb_extra.elf_name;
}

static inline uint64_t
CDL_SC_Period(CDL_Object *obj)
{
    return obj->sc_extra.period;
}

static inline uint64_t
CDL_SC_Budget(CDL_Object *obj)
{
    return obj->sc_extra.budget;
}

static inline seL4_Word
CDL_SC_Data(CDL_Object *obj)
{
    return obj->sc_extra.data;
}

static inline CDL_Core
CDL_SchedControl_Core(CDL_Object *obj)
{
    return obj->core;
}
#endif
