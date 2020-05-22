--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

-- Printer for C source format to be consumed by the CapDL initialiser.
-- Note: corresponds to the -c/--code argument.

module CapDL.PrintC where

import CapDL.Model
import CapDL.PrintUtils (sortObjects)

import Prelude ()
import Prelude.Compat
import Control.Exception (assert)
import Data.Ord (comparing)
import Data.List.Compat
import Data.List.Utils
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Bits
import Numeric (showHex)
import Text.PrettyPrint

-- What we print depends on whether objects are pre-allocated.
data AllocationType =
       StaticAlloc
     | DynamicAlloc ObjectSizeMap

(+++) :: String -> String -> String
s1 +++ s2 = s1 ++ "\n" ++ s2

joinBy :: String -> [String] -> String
joinBy = intercalate

hex :: Word -> String
hex x = "0x" ++ showHex x ""

-- This prints paddrs in hex, except that Nothing (no fixed address)
-- is simply "0" for clarity.
pointerOfPAddr :: Maybe Word -> String
pointerOfPAddr p = fromMaybe "0" (hex <$> p)

maxObjects :: Int -> String
maxObjects count = "#define MAX_OBJECTS " ++ show count

memberArch :: Arch -> String
memberArch arch =
    "#if !defined(CONFIG_ARCH_" ++ a ++ ")" +++
    "#    error \"invalid target architecture; expecting " ++ a ++ "\"" +++
    "#endif"
    where
        a = case arch of
            IA32 -> "IA32"
            ARM11 -> "ARM"
            X86_64 -> "X86_64"
            AARCH64 -> "AARCH64"
            RISCV -> "RISCV"

memberNum :: Int -> String
memberNum n = ".num = " ++ show n ++ ","

showObjID :: Map ObjID Int -> ObjID -> String
showObjID xs id = (case Map.lookup id xs of
    Just w -> show w
    _ -> "INVALID_SLOT") ++ " /* " ++ fst id ++ " */"

rightConst :: Rights -> String
rightConst = ("CDL_Can"++) . show

showRights :: CapRights -> String
showRights rights
  | Set.null rights = "0"
  | otherwise =
      "(" ++ joinBy "|" (rightConst <$> Set.toList rights) ++ ")"

showPCI :: Word -> (Word, Word, Word) -> String
showPCI domainID (pciBus, pciDev, pciFun) =
    hex $ shift domainID 16 .|. shift pciBus 8 .|. shift pciDev 3 .|. pciFun

-- Lookup-by-value on a dictionary. I feel like I need a shower.
lookupByValue :: (a -> Bool) -> Map k a -> k
lookupByValue f m = head $ Map.keys $ Map.filter f m

showCap :: Map ObjID Int -> Cap -> IRQMap -> String -> ObjMap Word -> String
showCap _ NullCap _ _ _ = "{.type = CDL_NullCap}"
showCap objs (UntypedCap id) _ _ _ =
    "{.type = CDL_UntypedCap, .obj_id = " ++ showObjID objs id ++ "}"
showCap objs (EndpointCap id badge rights) _ is_orig _ =
    "{.type = CDL_EPCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++
    ", .rights = " ++ showRights rights ++
    ", .data = { .tag = CDL_CapData_Badge, .badge = " ++ show badge ++ "}}"
showCap objs (NotificationCap id badge rights) _ is_orig _ =
    "{.type = CDL_NotificationCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++
    ", .rights = " ++ showRights rights ++
    ", .data = { .tag = CDL_CapData_Badge, .badge = " ++ show badge ++
    "}}"
showCap objs (ReplyCap id) _ _ _ =
    "{.type = CDL_ReplyCap, .obj_id = " ++ showObjID objs id ++ "}"
    -- XXX: Does it even make sense to give out a reply cap? How does init fake this?
showCap objs (MasterReplyCap id) _ _ _ =
    "{.type = CDL_MasterReplyCap, .obj_id = " ++ showObjID objs id ++ "}"
    -- XXX: As above.
showCap objs (CNodeCap id guard guard_size) _ is_orig _ =
    "{.type = CDL_CNodeCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++
    ", .rights = CDL_AllRights, .data = CDL_CapData_MakeGuard(" ++
    show guard_size ++ ", " ++ show guard ++ ")}"
showCap objs (TCBCap id) _ is_orig _ =
    "{.type = CDL_TCBCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++
    ", .rights = CDL_AllRights}"
showCap _ IRQControlCap _ _ _ = "{.type = CDL_IRQControlCap}"
showCap _ DomainCap _ _ _ = "{.type = CDL_DomainCap}"
showCap _ (IRQHandlerCap id) irqNode is_orig _ =
    "{.type = CDL_IRQHandlerCap, .obj_id = INVALID_OBJ_ID" ++
    ", .is_orig = " ++ is_orig ++
    ", .irq = " ++ show (lookupByValue (== id) irqNode) ++ "}"
    -- Caps have obj_ids, or IRQs, but not both.
showCap _ (IRQIOAPICHandlerCap id) irqNode is_orig _ =
    "{.type = CDL_IRQHandlerCap, .obj_id = INVALID_OBJ_ID" ++
    ", .is_orig = " ++ is_orig ++
    ", .irq = " ++ show (lookupByValue (== id) irqNode) ++ "}"
    -- Caps have obj_ids, or IRQs, but not both.
showCap _ (IRQMSIHandlerCap id) irqNode is_orig _ =
    "{.type = CDL_IRQHandlerCap, .obj_id = INVALID_OBJ_ID" ++
    ", .is_orig = " ++ is_orig ++
    ", .irq = " ++ show (lookupByValue (== id) irqNode) ++ "}"
showCap _ (ARMIRQHandlerCap id) irqNode is_orig _ =
    "{.type = CDL_IRQHandlerCap, .obj_id = INVALID_OBJ_ID" ++
    ", .is_orig = " ++ is_orig ++
    ", .irq = " ++ show (lookupByValue (== id) irqNode) ++ "}"
    -- Caps have obj_ids, or IRQs, but not both.
showCap objs (FrameCap id rights _ cached maybe_mapping) _ is_orig _ =
    "{.type = CDL_FrameCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++
    ", .rights = " ++ showRights rights ++
    ", .vm_attribs = " ++
          (if cached then "seL4_ARCH_Default_VMAttributes" else "CDL_VM_CacheDisabled") ++
    ", .mapping_container_id = " ++
          (case maybe_mapping of
               Just (mapping_container, _) -> showObjID objs mapping_container;
               _ -> "INVALID_OBJ_ID") ++
    ", .mapping_slot = " ++
          (case maybe_mapping of
               Just (_, mapping_slot) -> show mapping_slot;
               _ -> "0") ++
    "}"
    -- FIXME: I feel like I should be doing something with the ASID data here...
showCap objs (ARMSIDCap id) _ is_orig _ =
    "{.type = CDL_SIDCap, .obj_id = " ++ showObjID objs id ++ ", .is_orig = " ++ is_orig ++ "}"
showCap objs (ARMCBCap id) _ is_orig _ =
    "{.type = CDL_CBCap, .obj_id = " ++ showObjID objs id ++ ", .is_orig = " ++ is_orig ++ "}"

showCap objs (PTCap id _) _ is_orig _ =
    "{.type = CDL_PTCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"
showCap objs (PDCap id _) _ is_orig _ =
    "{.type = CDL_PDCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"

showCap objs (PDPTCap id _) _ is_orig _ =
    "{.type = CDL_PDPTCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"
showCap objs (PML4Cap id _) _ is_orig _ =
    "{.type = CDL_PML4Cap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"
showCap objs (PUDCap id _) _ is_orig _ =
    "{.type = CDL_PUDCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"
showCap objs (PGDCap id _) _ is_orig _ =
    "{.type = CDL_PGDCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"
showCap _ ASIDControlCap _ _ _ =
    "{.type = CDL_ASIDControlCap}"
showCap objs (ASIDPoolCap id) _ _ _ =
    "{.type = CDL_ASIDPoolCap, .obj_id = " ++ showObjID objs id ++ "}"
showCap objs (IOPortsCap id) _ is_orig _ =
    "{.type = CDL_IOPortsCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"
showCap objs (IOSpaceCap id) _ is_orig ms =
    "{.type = CDL_IOSpaceCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++
    ", .data = { .tag = CDL_CapData_Raw, .data = " ++ showPCI dom pci ++ "}}"
    where pci = pciDevice $ fromJust $ Map.lookup id ms
          dom = domainID $ fromJust $ Map.lookup id ms
showCap objs (ARMIOSpaceCap id) _ is_orig ms =
    "{.type = CDL_ARMIOSpaceCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++
    ", .data = { .tag = CDL_CapData_Raw, .data = " ++ show iospace ++ "}}"
    where iospace = armiospace $ fromJust $ Map.lookup id ms
showCap objs (VCPUCap id) _ _ _ = "{.type = CDL_VCPUCap, .obj_id = " ++ showObjID objs id ++ "}"
showCap _ (SchedControlCap affinity) _ _ _ =
    "{.type = CDL_SchedControlCap, .obj_id = " ++ (hex affinity) ++ " }"
showCap objs (RTReplyCap id) _ _ _ =
    "{.type = CDL_RTReplyCap, .obj_id = " ++ showObjID objs id ++ "}"
showCap objs (SCCap id) _ is_orig _ =
    "{.type = CDL_SCCap, .obj_id = " ++ showObjID objs id ++
    ", .is_orig = " ++ is_orig ++ "}"
showCap _ x _ _ _ = assert False $
    "UNSUPPORTED CAP TYPE: " ++ show x
    -- These are not supported by the initialiser itself.

showSlots :: Map ObjID Int -> ObjID -> [(Word, Cap)] -> IRQMap -> CDT -> ObjMap Word -> String
showSlots _ _ [] _ _ _ = ""
showSlots objs obj_id (x:xs) irqNode cdt ms =
    "{" ++ show index ++ ", " ++ slot ++ "}," +++
    showSlots objs obj_id xs irqNode cdt ms
    where
        index = fst x
        slot = showCap objs (snd x) irqNode is_orig ms
        is_orig = if Map.notMember (obj_id, index) cdt then "true" else "false"

memberSlots :: Map ObjID Int -> ObjID -> CapMap Word -> IRQMap -> CDT -> ObjMap Word -> String
memberSlots objs obj_id slots irqNode cdt ms =
    ".slots.num = " ++ show slot_count ++ "," +++
    ".slots.slot = (CDL_CapSlot[]) {" +++
    showSlots objs obj_id (Map.toList slots) irqNode cdt ms +++
    "},"
    where
        slot_count = Map.size slots

printInit :: [Word] -> String
printInit argv =
    "{" ++ joinBy ", " (map show argv) ++ "}"

showFrameFill :: [String] -> String
showFrameFill (dest_offset:dest_len:info_type:extra)  =
    "{.type = " ++ info_type ++ "," +++
    ".dest_offset = " ++ dest_offset ++ "," +++
    ".dest_len = " ++ dest_len ++ "," +++
    case (info_type,extra) of
        ("CDL_FrameFill_BootInfo",(bi_type:src_offset:[])) ->
            ".bi_type = {.type = " ++ bi_type ++ "," +++
            ".src_offset = " ++ src_offset +++
            "}},"
        ("CDL_FrameFill_FileData",(filename:file_offset:[])) ->
            ".file_data_type = {.filename = " ++ filename ++ "," +++
            ".file_offset = " ++ file_offset +++
            "}},"
        _ -> "#error Bad CDL_FrameFill_BootInfo args"
showFrameFill _  = ""

showFrameFills :: Maybe [[String]] -> String
showFrameFills (Just fills)  =
    ".fill = { " ++ unwords (map showFrameFill fills) ++ "}"
showFrameFills _ = ""


showFramePaddr :: Maybe Word -> String
showFramePaddr paddr =
    ".paddr = " ++ pointerOfPAddr paddr ++ ","

showObjectFields :: Map ObjID Int -> ObjID -> KernelObject Word -> IRQMap -> CDT -> ObjMap Word -> String
showObjectFields _ _ Endpoint _ _ _ = ".type = CDL_Endpoint,"
showObjectFields _ _ Notification _ _ _ = ".type = CDL_Notification,"
showObjectFields objs obj_id (TCB slots faultEndpoint info domain argv) _ _ _ =
    ".type = CDL_TCB," +++
    ".tcb_extra = {" +++
    "#if (" ++ hex ipcbuffer_addr ++ " & ((1 << seL4_IPCBufferSizeBits) - 1)) != 0" +++
    "#    error \"IPC buffer not correctly aligned\"" +++
    "#endif" +++
    ".ipcbuffer_addr = " ++ hex ipcbuffer_addr ++ "," +++
    ".priority = " ++ show priority ++ "," +++
    ".max_priority = " ++ show max_priority ++ "," +++
    ".affinity = " ++ show affinity ++ "," +++
    ".pc = " ++ hex pc ++ "," +++
    ".sp = " ++ hex stack ++ "," +++
    ".init = (const seL4_Word[])" ++ printInit argv ++ "," +++
    ".init_sz = " ++ show (length argv) ++ "," +++
    ".domain = " ++ show domain ++ "," +++
    ".resume = " ++ show resume' ++ "," +++
    ".fault_ep = " ++ show fault_ep ++ "," +++
    "}," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
    where
        ipcbuffer_addr = case info of {Just i -> ipcBufferAddr i; _ -> 0}
        priority = case info of {Just i -> case prio i of {Just p -> p; _ -> 125}; _ -> 125}
        max_priority = case info of {Just i -> case max_prio i of {Just p -> p; _ -> 125}; _ -> 125}
        affinity = case info of {Just i -> case affin i of {Just p -> p; _ -> 0}; _ -> 0}
        resume' = case info of {Just i -> case resume i of {Just True -> 1; Just False -> 0; _ -> 1}; _ -> 1}
        pc = case info of {Just i -> case ip i of {Just v -> v; _ -> 0}; _ -> 0}
        stack = case info of {Just i -> case sp i of {Just v -> v; _ -> 0}; _ -> 0}
        fault_ep = case faultEndpoint of {Just w -> w; _ -> 0}
showObjectFields objs obj_id (CNode slots sizeBits) irqNode cdt ms =
    ".type = " ++ t ++ "," +++
    ".size_bits = " ++ show sizeBits ++ "," +++
    memberSlots objs obj_id slots irqNode cdt ms
    where
        -- IRQs are represented in CapDL as 0-sized CNodes. This is fine for
        -- the model, but the initialiser needs to know what objects represent
        -- interrupts to avoid trying to create them at runtime. It's a bit of
        -- a hack to assume that any 0-sized CNode is an interrupt, but this is
        -- an illegal size for a valid CNode so everything should work out.
        t = if sizeBits == 0 then "CDL_Interrupt" else "CDL_CNode"
showObjectFields objs obj_id (IOAPICIrq slots ioapic pin level polarity) irqNode cdt ms =
    ".type = CDL_IOAPICInterrupt, " +++
    memberSlots objs obj_id slots irqNode cdt ms +++
    ".ioapicirq_extra = {" +++
        ".ioapic = " ++ show ioapic ++ "," +++
        ".ioapic_pin = " ++ show pin ++ "," +++
        ".level = " ++ show level ++ "," +++
        ".polarity = " ++ show polarity ++ "," +++
    "},"
showObjectFields objs obj_id (MSIIrq slots handle bus dev fun) irqNode cdt ms =
    ".type = CDL_MSIInterrupt, " +++
    memberSlots objs obj_id slots irqNode cdt ms +++
    ".msiirq_extra = {" +++
        ".handle = " ++ show handle ++ "," +++
        ".pci_bus = " ++ show bus ++ "," +++
        ".pci_dev = " ++ show dev ++ "," +++
        ".pci_fun = " ++ show fun ++ "," +++
    "},"
showObjectFields objs obj_id (ARMIrq slots trigger target) irqNode cdt ms =
    ".type = CDL_ARMInterrupt, " +++
    memberSlots objs obj_id slots irqNode cdt ms +++
    ".armirq_extra = {" +++
        ".trigger = " ++ show trigger ++ "," +++
        ".target = " ++ show target ++ "," +++
    "},"
showObjectFields _ _ (Untyped size_bits paddr) _ _ _ =
    ".type = CDL_Untyped," +++
    ".size_bits = " ++ maybe "-1" show size_bits ++ "," +++
    ".paddr = " ++ pointerOfPAddr paddr ++ ","
showObjectFields objs obj_id (PT slots) _ _ _ =
    ".type = CDL_PT," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
showObjectFields objs obj_id (PD slots) _ _ _ =
    ".type = CDL_PD," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
showObjectFields objs obj_id (PDPT slots) _ _ _ =
    ".type = CDL_PDPT," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
showObjectFields objs obj_id (PML4 slots) _ _ _ =
    ".type = CDL_PML4," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
showObjectFields objs obj_id (PUD slots) _ _ _ =
    ".type = CDL_PUD," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
showObjectFields objs obj_id (PGD slots) _ _ _ =
    ".type = CDL_PGD," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
showObjectFields _ _ (Frame size paddr extra) _ _ _ =
    ".type = CDL_Frame," +++
    ".size_bits = " ++ show size ++ "," +++
    ".frame_extra = { " ++ showFramePaddr paddr ++ showFrameFills extra +++ " },"
showObjectFields _ _ (IOPorts (start, end)) _ _ _ =
    ".type = CDL_IOPorts," +++
    ".start = " ++ show start ++ "," ++
    ".end = " ++ show end ++ ","
showObjectFields objs obj_id (ASIDPool slots asidHigh) _ _ _ =
    ".type = CDL_ASIDPool," +++
    ".asid_high = " ++ maybe "-1" hex asidHigh ++ "," +++
    memberSlots objs obj_id slots Map.empty Map.empty Map.empty -- IRQ, cdt and obj map not required
showObjectFields _ _ IODevice{} _ _ _ =
    ".type = CDL_IODevice,"
showObjectFields _ _ ARMIODevice{} _ _ _ =
    ".type = CDL_ARMIODevice,"
showObjectFields _ _ VCPU _ _ _ = ".type = CDL_VCPU,"
showObjectFields _ _ (SC info size_bits) _ _ _ =
    ".type = CDL_SchedContext," +++
    ".sc_extra = {" +++
        ".period = " ++ show sc_period ++ "," +++
        ".budget = " ++ show sc_budget ++ "," +++
        ".data = " ++ show sc_data ++ "," +++
    "}," +++
    ".size_bits = " ++ show sizeBits ++ ","
    where
    sc_period = fromMaybe 0 (maybe Nothing period info)
    sc_budget = fromMaybe 0 (maybe Nothing budget info)
    sc_data   = fromMaybe 0 (maybe Nothing scData info)
    sizeBits  = fromMaybe 0 size_bits
showObjectFields _ _ (RTReply {}) _ _ _ = ".type = CDL_RTReply,"
showObjectFields _ _ (ARMSID {}) _ _ _ = ".type = CDL_SID,"
showObjectFields _ _ (ARMCB {}) _ _ _ = ".type = CDL_CB,"
showObjectFields _ _ x _ _ _ = assert False $
    "UNSUPPORTED OBJECT TYPE: " ++ show x

showObject :: Map ObjID Int -> (ObjID, KernelObject Word) -> IRQMap -> CDT -> ObjMap Word -> String
showObject objs obj irqNode cdt ms =
    "{" +++
    "#ifdef CONFIG_DEBUG_BUILD" +++ ".name = \"" ++ name ++ "\"," +++ "#endif" +++
    showObjectFields objs id (snd obj) irqNode cdt ms +++
    "}"
    where
        id = fst obj
        name = fst id ++ (case snd id of
            Just index -> "[" ++ show index ++ "]"
            _ -> "")

showObjects :: Map ObjID Int -> Int -> [(ObjID, KernelObject Word)] -> IRQMap -> CDT -> ObjMap Word -> String
showObjects _ _ [] _ _ _ = ""
showObjects objs counter (x:xs) irqNode cdt ms =
    "[" ++ show counter ++ "] = " ++ showObject objs x irqNode cdt ms ++ "," +++
    showObjects objs (counter + 1) xs irqNode cdt ms

memberObjects :: Map ObjID Int -> [(ObjID, KernelObject Word)] -> IRQMap -> CDT ->
                 ObjMap Word -> String
memberObjects obj_ids obj_list irqNode cdt objs =
    ".objects = (CDL_Object[]) {" +++
    showObjects obj_ids 0 obj_list irqNode cdt objs +++
    "},"

-- Emit an array where each entry represents a given interrupt. Each is -1 if
-- that interrupt has no handler or else the object ID of the interrupt
-- (0-sized CNode).
memberIRQs :: Map ObjID Int -> IRQMap -> Arch -> String
memberIRQs objs irqNode _ =
    let maxIrq = case Map.null irqNode of
            False -> fst $ Map.findMax irqNode
            True -> 0
    in
    ".num_irqs = " ++ show (maxIrq + 1) ++ "," +++
    ".irqs = (CDL_ObjID[]){" +++
    joinBy ", "
        [ case Map.lookup k irqNode of
              Just i -> show $ fromJust $ Map.lookup i objs
              _ -> "-1"
        | k <- [0 .. maxIrq]
        ] +++
    "},"

showUntypedDerivation :: Map ObjID Int -> ObjID -> [ObjID] -> String
showUntypedDerivation objs utID utChildren =
    "{" +++
    "  .untyped = " ++ showObjID objs utID ++ "," +++
    "  .num = " ++ show (length utChildren) ++ "," +++
    "  .children = (CDL_ObjID[]){" +++
    Data.List.Utils.join ",\n"
        ["    " ++ showObjID objs childID | childID <- utChildren] +++
    "  }" +++
    "}"

showUntypedDerivations :: AllocationType -> Map ObjID Int -> CoverMap -> String
showUntypedDerivations DynamicAlloc{} _ untypedCovers
  | all null (Map.elems untypedCovers) =
      ".num_untyped = 0," +++
      ".untyped = NULL,"
  | otherwise = error $
      "refusing to generate spec for dynamic allocation because the " ++
      "following untypeds already have children:\n" ++
      Data.List.Utils.join "\n"
        [ "  " ++ show utID
        | (utID, utChildren) <- Map.toList untypedCovers
        , not $ null utChildren ]
showUntypedDerivations StaticAlloc objs untypedCovers =
    ".num_untyped = " ++ show (Map.size untypedCovers) ++ "," +++
    ".untyped = (CDL_UntypedDerivation[]){" +++
    joinBy ",\n" (map (uncurry (showUntypedDerivation objs)) $
                  Map.toList untypedCovers) +++
    "},"

-- find all ASIDPools and prepare them for allocation wrt. asid_high.
getASIDPoolDerivations :: ObjMap Word -> [(Word, ObjID)]
getASIDPoolDerivations ms =
    let table =
          sortBy (comparing fst)
              [ (asidHigh, objID)
              | (objID, ASIDPool lowSlots maybeAsidHigh) <- Map.toList ms,
                -- sanity checks
                Map.null lowSlots
                    || can'tAllocate objID "it has nonempty low slots",
                maybeAsidHigh /= Nothing
                    || can'tAllocate objID "it has no assigned asid_high",
                let Just asidHigh = maybeAsidHigh
              ]
        asidHighs = map fst table
    in -- more sanity checks
       if asidHighs /= [1..fromIntegral (length asidHighs)]
       then error $ "ASID pools don't have slot numbers of the form [1..n]: " ++ show table
       else table
    where can'tAllocate objID reason =
              error $ "can't allocate ASID pool " ++ show objID ++ " because " ++ reason

showASIDPoolDerivations :: Map ObjID Int -> ObjMap Word -> String
showASIDPoolDerivations objs ms =
    let table = getASIDPoolDerivations ms
        -- include ignored slot 0
        array = "(CDL_ObjID)-1 /* slot reserved for root thread, ignored */"
                : map (showObjID objs . snd) table
    in ".num_asid_slots = " ++ show (length array) ++ "," +++
       ".asid_slots = (CDL_ObjID[]){" +++
           joinBy ",\n" ["    " ++ idStr | idStr <- array] +++
       "},"

printC :: AllocationType -> Model Word -> Idents CapName -> CopyMap -> Doc
printC allocType (Model arch objs irqNode cdt untypedCovers) _ _ =
    text $
    "/* Generated file. Your changes will be overwritten. */" +++
    "" +++
    "#include <capdl.h>" +++
    "#include <sel4/sel4.h>" +++
    "" +++
    "#ifndef INVALID_SLOT" +++
    "#define INVALID_SLOT (-1)" +++
    "#endif" +++
    "" +++
    maxObjects objs_sz +++ -- FIXME: I suspect this is not the right list to measure.
    "" +++
    "CDL_Model capdl_spec = {" +++
    memberArch arch +++
    memberNum objs_sz +++
    memberIRQs obj_ids irqNode arch +++
    memberObjects obj_ids obj_list irqNode cdt objs +++
    showUntypedDerivations allocType obj_ids untypedCovers +++
    showASIDPoolDerivations obj_ids objs +++
    "};"
    where objs_sz = length $ Map.toList objs
          obj_list = case allocType of
              StaticAlloc -> Map.toList objs
              DynamicAlloc objSizeMap -> sortObjects objSizeMap (Map.toList objs)
          obj_ids = Map.fromList $ flip zip [0..] $ map fst obj_list
