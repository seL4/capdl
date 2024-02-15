--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module CapDL.Model where

import Prelude ()
import Prelude.Compat
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Data
import Data.Word
import Control.Lens

-- Supported architectures:
data Arch = IA32 | ARM11 | X86_64 | AARCH64 | RISCV deriving (Eq, Show)

-- Access rights of capabilities. Not all capability types support all rights.
data Rights = Read
            | Write
            | Grant
            | GrantReply
            deriving (Eq, Show, Enum, Ord, Typeable, Data)

type CapRights = Set Rights

allRights :: CapRights
allRights = Set.fromList [Read, Write, Grant, GrantReply]

--
-- Object identifiers.
--
-- Currently, each object is described by a string (its name), possibly
-- followed by a word (an index). The latter feature allows arrays of
-- objects to be specified without having to give them explicit names
-- (for instance, when declaring large numbers of frames for a process).
--
-- These may later be converted to a type variable.
--
type ObjID = (String, Maybe Word)

type Asid = (Word, Word)

--
-- Capabilities.
--
-- Each cap has a type (what the cap does), and a bunch of attributes
-- (such as the rights, what object the capability gives access to,
-- etc).
--
data Cap
        = NullCap
        | UntypedCap { capObj :: ObjID }
        | EndpointCap {
            capObj :: ObjID,
            capBadge :: Word,
            capRights :: CapRights }
        | NotificationCap {
            capObj :: ObjID,
            capBadge :: Word,
            capRights :: CapRights }
        | ReplyCap { capObj :: ObjID }
        | MasterReplyCap { capObj :: ObjID }
        | CNodeCap {
            capObj :: ObjID,
            capGuard :: Word,
            capGuardSize :: Word }
        | TCBCap { capObj :: ObjID }
        | IRQControlCap
        | IRQHandlerCap { capObj :: ObjID }
        | IRQIOAPICHandlerCap {
            capObj :: ObjID }
        | IRQMSIHandlerCap {
            capObj :: ObjID }
        | DomainCap
        | SCCap { capObj :: ObjID }
        | SchedControlCap { core :: Word }
        | RTReplyCap { capObj :: ObjID }
        | VCPUCap {
            capObj :: ObjID }

        -- arch specific caps, ARM11, IA32, X86_64 and AARCH64 merged
        | FrameCap {
            capObj :: ObjID,
            capRights :: CapRights,
            capMaybeAsid :: Maybe Asid,
            capCached :: Bool,
            capMaybeMapping :: Maybe (ObjID, Word) }
        | PTCap {
            capObj :: ObjID,
            capMaybeAsid :: Maybe Asid }
        | PDCap {
            capObj :: ObjID,
            capMaybeAsid :: Maybe Asid }
        | PDPTCap {
            capObj :: ObjID,
            capMaybeAsid :: Maybe Asid }
        | PML4Cap {
            capObj :: ObjID,
            capMaybeAsid :: Maybe Asid }
        | PUDCap {
            capObj :: ObjID,
            capMaybeAsid :: Maybe Asid }
        | PGDCap {
            capObj :: ObjID,
            capMaybeAsid :: Maybe Asid }
        | ASIDControlCap -- only one ASIDTable in the system
        | ASIDPoolCap {
            capObj :: ObjID }

        -- ARM specific caps
        | ARMIOSpaceCap    { capObj :: ObjID }
        | ARMIRQHandlerCap { capObj :: ObjID }
        | ARMSIDCap        { capObj :: ObjID }
        | ARMCBCap         { capObj :: ObjID }
        | ARMSMCCap        {
            capObj :: ObjID,
            capBadge :: Word }

        -- X86 specific caps
        | IOPortsCap {
            capObj :: ObjID}
        | IOSpaceMasterCap -- can mint to any IOSpaceCap
        | IOSpaceCap { capObj :: ObjID }
        | IOPTCap { capObj :: ObjID }

        deriving (Eq, Ord, Show)

-- Use Template Haskell to create Prisms corresponding to the Cap constructors.
-- In combination with "Control.Lens.Extras (is)" this allows us to easily write
-- things like "is _CNodeCap".
makePrisms ''Cap

-- Kernel Objects

type CapMap a = Map a Cap

data TCBExtraInfo = TCBExtraInfo {
    ipcBufferAddr :: Word,
    ip :: Maybe Word,
    sp :: Maybe Word,
    prio :: Maybe Integer,
    max_prio :: Maybe Integer,
    affin :: Maybe Integer,
    resume :: Maybe Bool }
    deriving (Eq, Show)

data SCExtraInfo = SCExtraInfo {
    period :: Maybe Word64,
    budget :: Maybe Word64,
    scData :: Maybe Word }
    deriving (Eq, Show)

--
-- Kernel objects in memory.
--
-- This type represents attributes associated with in-memory kernel
-- objects. The type parameter 'a' is the type used to name caps. (For
-- example, a C implementation would use a 32-bit word, while another
-- implementation may pre-decode caps in a list of words).
--
data KernelObject a
    = Endpoint
    | Notification
    | TCB {
        slots :: CapMap a,
        faultEndpoint :: Maybe Word,
        extraInfo :: Maybe TCBExtraInfo,
        dom :: Integer,
        initArguments :: [Word] }
    | CNode {
        slots :: CapMap a,
        sizeBits :: Word }
    | Untyped {
        maybeSizeBits :: Maybe Word,
        maybePaddr :: Maybe Word }
    | SC {
        sc_extraInfo :: Maybe SCExtraInfo,
        maybeSizeBits :: Maybe Word}
    | RTReply
    | VCPU

-- arch specific objects, ARM11, IA32, X86_64, AARCH64 and RISCV mixed
    | ASIDPool { slots :: CapMap a,
                 capAsidHigh :: Maybe Word }
    | PT { slots :: CapMap a }
    | PD { slots :: CapMap a }
    | PDPT { slots :: CapMap a }
    | PML4 { slots :: CapMap a }
    | PUD { slots :: CapMap a }
    | PGD { slots :: CapMap a }
    | Frame {
        vmSizeBits :: Word,
        maybePaddr :: Maybe Word,
        maybeFill :: Maybe [[String]] }

-- ARM specific objects
    | ARMIODevice {
        slots  :: CapMap a,
        armiospace :: Word}
    | ARMIrq {
        slots :: CapMap a,
        trigger :: Word,
        target :: Word }

-- fake kernel objects for smmu
    | ARMSID
    | ARMCB { bankNumber :: Maybe Word }

    | ARMSMC

-- X86 specific objects
    | IOPorts { ports :: (Word, Word) }
    | IODevice {
        slots :: CapMap a,
        domainID :: Word,
        pciDevice :: (Word, Word, Word)}
    | IOPT {
        slots :: CapMap a,
        level :: Word }
    | IOAPICIrq {
        slots :: CapMap a,
        ioapic :: Word,
        ioapic_pin :: Word,
        ioapic_level :: Word,
        ioapic_polarity :: Word }
    | MSIIrq {
        slots :: CapMap a,
        handle :: Word,
        bus :: Word,
        dev :: Word,
        fun :: Word }
    deriving (Eq, Show)

objPaddr :: KernelObject a -> Maybe Word
objPaddr (Frame _ paddr _) = paddr
objPaddr (Untyped _ paddr) = paddr
objPaddr _ = Nothing

data KOType
    = Endpoint_T
    | Notification_T
    | TCB_T
    | CNode_T
    | Untyped_T
    | IrqSlot_T
    | IOAPICIrqSlot_T
    | MSIIrqSlot_T
    | ARMIrqSlot_T
    | ARMSID_T
    | ARMCB_T
    | ARMSMC_T
    | ASIDPool_T
    | PT_T
    | PD_T
    | PDPT_T
    | PML4_T
    | PUD_T
    | PGD_T
    | Frame_T
    | IOPorts_T
    | IODevice_T
    | ARMIODevice_T
    | IOPT_T
    | VCPU_T
    | SC_T
    | RTReply_T
    deriving (Show, Eq, Ord, Enum)

-- Lookup table of object sizes, to be passed in externally.
type ObjectSizeMap = Map KOType Word

--
-- A reference to a capability.
--
-- The ObjID is the kernel object the capability sits within (which will
-- be either a CNode or a TCB), and the Word represents the slot
-- (indexed from 0).
--
type CapRef = (ObjID, Word)

-- The name of a cap, used when copying caps.
type CapName = ObjID

type ObjMap a = Map ObjID (KernelObject a)

type IRQMap = Map Word ObjID

-- Map from untypeds to the lists of objects they cover.
-- Note that we use lists to preserve ordering. The Python capDL
-- allocator uses ordering to describe where objects are to be allocated
-- within each untyped.
--
-- This list may contain duplicate IDs while parsing; see
-- 'CapDL.MakeModel.dedupCoverIDs'.
type CoverMap = Map ObjID [ObjID]

-- UTs without cover decls aren't recorded, so return '[]' for those
getUTCover :: ObjID -> CoverMap -> [ObjID]
getUTCover = Map.findWithDefault []

type CDT = Map CapRef CapRef

--
-- The state of the system.
--
-- The system state consists of:
--
--   1. The architecture in use;
--   2. The objects currently present;
--   3. The global irq node; and
--   4. The cap derivation tree (which allows us to determine which
--      objects are derived from which other objects)
--
-- Two forms of model exist. The first is an abstract model where
-- CSpaces are assumed to be flat. Caps are identified by a Word
-- pointing somewhere in the CSpace.
--
-- The second model is a more concrete object where caps are assumed to
-- be in a tree of CNodes. Caps are identified by a list of Words
-- indicating the offsets of the target cap in each level of the tree.
--
data Model a
  = Model {
    arch :: Arch,
    objects :: ObjMap a,
    irqNode :: IRQMap,
    cdt :: CDT,
    untypedCovers :: CoverMap }
  deriving Show

data Idents cap_id = Idents {
    cap_ids :: Map cap_id CapRef
} deriving Show

type CopyMap = Map CapRef CapName

--
-- Each TCB contains several cap slots. The following constants define the
-- slot numbers in which they will be found if the TCB is treated as
-- a CNode.
--
tcbCTableSlot :: Word
tcbCTableSlot = 0

tcbVTableSlot :: Word
tcbVTableSlot = 1

tcbReplySlot :: Word
tcbReplySlot = 2

tcbCallerSlot :: Word
tcbCallerSlot = 3

tcbIPCBufferSlot :: Word
tcbIPCBufferSlot = 4

tcbFaultEPSlot :: Word
tcbFaultEPSlot = 5

tcbSCSlot :: Word
tcbSCSlot = 6

tcbTempFaultEPSlot :: Word
tcbTempFaultEPSlot = 7

tcbBoundNotificationSlot :: Word
tcbBoundNotificationSlot = 8

tcbBoundVCPUSlot :: Word
tcbBoundVCPUSlot = 9

--
-- The string used when defining an IOSpaceMasterCap, an ASIDControlCap,
-- an IRQControlCap, a DomainCap or a SchedControlCap.
--
ioSpaceMaster :: String
ioSpaceMaster = "io_space_master"

asidControl :: String
asidControl = "asid_control"

irqControl :: String
irqControl = "irq_control"

domain :: String
domain = "domain"

schedControl :: String
schedControl = "sched_control"

capStrings :: [String]
capStrings = [ioSpaceMaster, asidControl, irqControl, domain, schedControl]

--
-- Determine if the given capability points to an object.
--
hasObjID :: Cap -> Bool
hasObjID NullCap = False
hasObjID IOSpaceMasterCap = False
hasObjID ASIDControlCap = False
hasObjID IRQControlCap = False
hasObjID DomainCap = False
hasObjID (SchedControlCap {}) = False
hasObjID _  = True

--
-- Get the object a particular cap points to.
--
-- This function is partial, not all caps point to an object.
--
objID :: Cap -> ObjID
objID = capObj

--
-- Determine if the given cap has rights.
--
hasRights :: Cap -> Bool
hasRights (NotificationCap {})   = True
hasRights (EndpointCap {})        = True
hasRights (ARMSMCCap {})          = True
hasRights (FrameCap {})           = True
hasRights _                       = False

--
-- Determine if the given object has capability slots.
--
hasSlots :: KernelObject a -> Bool
hasSlots (TCB {})       = True
hasSlots (CNode {})     = True
hasSlots (ASIDPool {})  = True
hasSlots (PT {})        = True
hasSlots (PD {})        = True
hasSlots (PDPT {})      = True
hasSlots (PML4 {})      = True
hasSlots (PUD {})      = True
hasSlots (PGD {})      = True
hasSlots (ARMIODevice {}) = True
hasSlots (ARMIrq {})    = True
hasSlots (IODevice {})  = True
hasSlots (IOPT {})      = True
hasSlots (IOAPICIrq {}) = True
hasSlots (MSIIrq {})    = True
hasSlots _              = False
