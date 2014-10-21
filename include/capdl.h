/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __CAPDL_H__
#define __CAPDL_H__

#include <sel4/types.h>
#include <autoconf.h>
#include <stdbool.h>

#if defined(ARCH_ARM)

#if defined(ARM_HYP)
#define PT_SIZE          9
#else
#define PT_SIZE          8
#endif
#define FRAME_SIZE       12

#define seL4_Page_Map               seL4_ARM_Page_Map
#define seL4_PageTable_Map          seL4_ARM_PageTable_Map
#define seL4_Page_Unmap             seL4_ARM_Page_Unmap
#define seL4_PageTable_Unmap        seL4_ARM_PageTable_Unmap
#define seL4_Default_VMAttributes   seL4_ARM_Default_VMAttributes
#define seL4_ASIDPool_Assign        seL4_ARM_ASIDPool_Assign
#define seL4_ASIDControl_MakePool   seL4_ARM_ASIDControl_MakePool
#define seL4_VMAttributes           seL4_ARM_VMAttributes

#define CDL_VM_CacheEnabled         seL4_ARM_PageCacheable
#define CDL_VM_CacheDisabled        seL4_ARM_ParityEnabled
#define CDL_VM_WriteThrough         0

#elif defined(ARCH_IA32)

#define PT_SIZE          10
#define FRAME_SIZE       12

#define seL4_Page_Map               seL4_IA32_Page_Map
#define seL4_PageTable_Map          seL4_IA32_PageTable_Map
#define seL4_Page_Unmap             seL4_IA32_Page_Unmap
#define seL4_PageTable_Unmap        seL4_IA32_PageTable_Unmap
#define seL4_Default_VMAttributes   seL4_IA32_Default_VMAttributes
#define seL4_ASIDPool_Assign        seL4_IA32_ASIDPool_Assign
#define seL4_ASIDControl_MakePool   seL4_IA32_ASIDControl_MakePool
#define seL4_VMAttributes           seL4_IA32_VMAttributes

#define CDL_VM_CacheEnabled         0
#define CDL_VM_CacheDisabled        seL4_IA32_CacheDisabled
#define CDL_VM_WriteThrough         seL4_IA32_WriteThrough

#endif

/* Binary CapDL representation -- capdl.h */

/* Arch: Supported architectures: */
typedef enum {
#if defined(ARCH_ARM)
    CDL_Arch_ARM
#elif defined(ARCH_IA32)
    CDL_Arch_IA32
#endif
} CDL_Arch;

/* CapRights:  Access rights of capabilities */
typedef seL4_CapRights CDL_CapRights;

/* ObjectID: index into the objects array */
typedef seL4_Word CDL_ObjID;
#define INVALID_OBJ_ID ((CDL_ObjID)-1)

/* IRQ number: Hardware IRQ number */
typedef seL4_Word CDL_IRQ;

/* Capability: */
typedef enum {
    CDL_NullCap,
    CDL_UntypedCap,
    CDL_EPCap,
    CDL_AEPCap,
    CDL_ReplyCap,
    CDL_MasterReplyCap,
    CDL_CNodeCap,
    CDL_TCBCap,
    CDL_IRQControlCap,
    CDL_IRQHandlerCap,
    CDL_FrameCap,
    CDL_PTCap,
    CDL_PDCap,
    CDL_ASIDControlCap,
    CDL_ASIDPoolCap,
#if defined(ARCH_IA32)
    CDL_IOPortsCap,
    CDL_IOSpaceCap,
#endif
} CDL_CapType;

typedef struct {
    enum {
        CDL_CapData_Badge = seL4_CapData_Badge,
        CDL_CapData_Guard = seL4_CapData_Guard,
        CDL_CapData_Raw,
    } tag;
    uint32_t guard_bits;
    uint32_t guard_size;
    uint32_t badge;
    uint32_t data;
} CDL_CapData;

typedef struct {
    CDL_CapType type;
    CDL_ObjID obj_id;
    CDL_CapData data;
    CDL_IRQ irq;
    bool is_orig;
    seL4_VMAttributes vm_attribs;
    seL4_CapRights rights;
} CDL_Cap;

/* CapMap: is just an array of cap slots, position of the slot and cap */
typedef struct {
    seL4_Word slot;
    CDL_Cap cap;
} CDL_CapSlot;

typedef struct {
    seL4_Word num;
    CDL_CapSlot *slot;
} CDL_CapMap;

/* ObjIDSet: is just an array of object ids */
typedef struct {
    seL4_Word num;
    CDL_ObjID *slot;
} CDL_ObjIDSet;

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
    CDL_AsyncEndpoint = seL4_AsyncEndpointObject,
    CDL_TCB           = seL4_TCBObject,
    CDL_CNode         = seL4_CapTableObject,
    CDL_Untyped       = seL4_UntypedObject,
#if defined(ARCH_ARM)
    CDL_PT            = seL4_ARM_PageTableObject,
    CDL_PD            = seL4_ARM_PageDirectoryObject,
    CDL_Frame         = seL4_ARM_SmallPageObject,
#elif defined(ARCH_IA32)
    CDL_PT            = seL4_IA32_PageTableObject,
    CDL_PD            = seL4_IA32_PageDirectoryObject,
    CDL_Frame         = seL4_IA32_4K,
#endif
    CDL_ASIDPool      = seL4_ObjectTypeCount + 1,
    CDL_Interrupt     = seL4_ObjectTypeCount + 2,
#if defined(ARCH_IA32)
    CDL_IOPorts       = seL4_ObjectTypeCount + 3,
    CDL_IODevice      = seL4_ObjectTypeCount + 4,
#endif
} CDL_ObjectType;

typedef struct {
    seL4_Word ipcbuffer_addr;
    void *driverinfo;
    seL4_Word driverinfo_addr;
    uint8_t priority;
    seL4_Word pc;
    seL4_Word sp;
    char *elf_name;
    const seL4_Word *init;
    seL4_Word init_sz;
    uint32_t domain;
} CDL_TCBExtraInfo;

typedef struct {
    char *name; /* textual ObjID from the capDL spec */

    CDL_ObjectType type;
    CDL_CapMap slots;
    seL4_Word size_bits;
    CDL_TCBExtraInfo tcb_extra;

    CDL_ObjMap covers; /* XXX: should this be in a separate coversMap? */

    void *paddr; /* Physical address; only relevant for frames. */
} CDL_Object;

/* CapDLModel: is described by a map from ObjectIDs (array index) to Objects */
typedef struct {
    CDL_Arch arch;

    seL4_Word num;
    CDL_Object *objects;

    CDL_ObjID irqs[CONFIG_CAPDL_LOADER_MAX_IRQS];

    /* covers: the root untyped TODO: change to CoverMap? */
    CDL_ObjID root_ut;
} CDL_Model;

/* helper functions ---------------------------------------------------------------------------- */

#define CDL_R                       seL4_CanRead
#define CDL_W                       seL4_CanWrite
#define CDL_G                       seL4_CanGrant
#define CDL_WG                      (CDL_W | CDL_G)
#define CDL_RG                      (CDL_R | CDL_G)
#define CDL_RW                      (CDL_R | CDL_W)
#define CDL_RWG                     (CDL_R | CDL_W | CDL_G)

#define CDL_TCB_CTable_Slot         0
#define CDL_TCB_VTable_Slot         1
#define CDL_TCB_Reply_Slot          2
#define CDL_TCB_Caller_Slot         3
#define CDL_TCB_IPCBuffer_Slot      4
#define CDL_TCB_FaultEP_Slot        5

#define CDL_CapData_MakeGuard(x, y) \
{ .tag = seL4_CapData_Guard, .guard_bits = (y), .guard_size = (x) }

#define CDL_CapData_MakeBadge(x)    { .tag = seL4_CapData_Badge, .badge = (x) }

static inline CDL_CapType    CDL_Cap_Type(CDL_Cap *cap)                   { return cap->type; }
static inline CDL_CapData    CDL_Cap_Data(CDL_Cap *cap)                   { return cap->data; }
static inline seL4_VMAttributes CDL_Cap_VMAttributes(CDL_Cap *cap)        { return cap->vm_attribs; }
static inline CDL_ObjID      CDL_Cap_ObjID(CDL_Cap *cap)                  { return cap->obj_id; }
static inline CDL_CapRights  CDL_Cap_Rights(CDL_Cap *cap)                 { return cap->rights; }
static inline CDL_IRQ        CDL_Cap_IRQ(CDL_Cap *cap)                    { return cap->irq; }
static inline bool           CDL_Cap_IsOrig(CDL_Cap *cap)                 { return cap->is_orig; }


static inline seL4_Word      CDL_CapSlot_Slot(CDL_CapSlot *cap_slot)      { return cap_slot->slot; }
static inline CDL_Cap *      CDL_CapSlot_Cap(CDL_CapSlot *cap_slot)       { return &cap_slot->cap; }

static inline seL4_Word      CDL_ObjSlot_Slot(CDL_ObjSlot *obj_slot)      { return obj_slot->slot; }
static inline CDL_ObjID      CDL_ObjSlot_ObjID(CDL_ObjSlot *obj_slot)     { return obj_slot->id; }

static inline char *         CDL_Obj_Name(CDL_Object *obj)                { return obj->name; }
static inline CDL_ObjectType CDL_Obj_Type(CDL_Object *obj)                { return obj->type; }
static inline seL4_Word      CDL_Obj_SizeBits(CDL_Object *obj)            { return obj->size_bits; }
static inline seL4_Word      CDL_Obj_NumSlots(CDL_Object *obj)            { return obj->slots.num; }
static inline CDL_CapSlot *  
CDL_Obj_GetSlot(CDL_Object *obj, seL4_Word i)      { return &obj->slots.slot[i]; }

static inline seL4_Word      
CDL_Untyped_NumSlots(CDL_Object *obj)        { return obj->covers.num; }

static inline CDL_ObjSlot *  
CDL_Untyped_GetSlot(CDL_Object *obj, seL4_Word i)  { return &obj->covers.slot[i]; }

static inline void *     
CDL_TCB_DriverInfo(CDL_Object *obj)          { return obj->tcb_extra.driverinfo; }

static inline seL4_Word      
CDL_TCB_DriverInfo_Addr(CDL_Object *obj)     { return obj->tcb_extra.driverinfo_addr; }
              
static inline seL4_Word      
CDL_TCB_IPCBuffer_Addr(CDL_Object *obj)      { return obj->tcb_extra.ipcbuffer_addr; }

static inline uint8_t
CDL_TCB_Priority(CDL_Object *obj)            { return obj->tcb_extra.priority; }

static inline uint32_t
CDL_TCB_Domain(CDL_Object *obj)             { return obj->tcb_extra.domain; }

static inline seL4_Word      
CDL_TCB_PC(CDL_Object *obj)                  { return obj->tcb_extra.pc; }

static inline seL4_Word      
CDL_TCB_SP(CDL_Object *obj)                  { return obj->tcb_extra.sp; }

static inline char *
CDL_TCB_ElfName(CDL_Object *obj)             { return obj->tcb_extra.elf_name; }

#endif
