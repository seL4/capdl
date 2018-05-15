--
-- Copyright 2017, Data61
-- Commonwealth Scientific and Industrial Research Organisation (CSIRO)
-- ABN 41 687 119 230.
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(DATA61_BSD)
--

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module CapDL.PrintUtils where

import CapDL.Model

import Text.PrettyPrint
import Prelude ()
import Prelude.Compat
import qualified Data.Set as Set
import Data.Word
import Numeric
import Data.Maybe
import Data.List
import Data.List.Utils

listSucc :: Enum a => [a] -> [a]
listSucc list = init list ++ [succ (last list)]

class (Show a, Eq a) => Printing a where
    isSucc :: a -> a -> Bool
    num :: a -> Doc

instance Printing Word where
    isSucc first second = succ first == second
    num n = int (fromIntegral n)

instance Printing [Word] where
    isSucc first second = listSucc first == second
    num ns = hsep $ punctuate comma (map num ns)

hex :: Word -> String
hex x = "0x" ++ showHex x ""

--Horrible hack for integral log base 2.
logBase2 :: Word -> Int -> Int
logBase2 1 i = i
logBase2 n i = logBase2 (n `div` 2) (i+1)

showID :: ObjID -> String
showID (name, Nothing) = name
showID (name, Just num) = name ++ "[" ++ show num ++ "]"

maybeParens text
    | isEmpty text = empty
    | otherwise = parens text

maybeParensList text =
  maybeParens $ hsep $ punctuate comma $ filter (not . isEmpty) text

prettyBits bits = num bits <+> text "bits"

prettyMBits mbits =
    case mbits of
        Nothing -> empty
        Just bits -> prettyBits bits

prettyLevel l = text "level" <> colon <+> num l

--Is there a better way to do this?
prettyVMSize vmSz =
    if vmSz >= 2^20
    then num (vmSz `div` (2^20)) <> text "M"
    else num (vmSz `div` (2^10)) <> text "k"

prettyPaddr :: Maybe Word -> Doc
prettyPaddr Nothing = empty
prettyPaddr (Just p) = text "paddr:" <+> (text $ hex p)

prettyAddr :: Word -> Doc
prettyAddr addr = text "addr:" <+> num addr

prettyIP :: Maybe Word -> Doc
prettyIP Nothing = empty
prettyIP (Just ip) = text "ip:" <+> num ip

prettySP :: Maybe Word -> Doc
prettySP Nothing = empty
prettySP (Just sp) = text "sp:" <+> num sp

prettyElf :: Maybe String -> Doc
prettyElf Nothing = empty
prettyElf (Just elf) = text "elf:" <+> text elf

prettyPrio :: Maybe Integer -> Doc
prettyPrio Nothing = empty
prettyPrio (Just prio) = text "prio:" <+> (text $ show prio)

prettyMaxPrio :: Maybe Integer -> Doc
prettyMaxPrio Nothing = empty
prettyMaxPrio (Just max_prio) = text "max_prio:" <+> (text $ show max_prio)

prettyAffinity :: Maybe Integer -> Doc
prettyAffinity Nothing = empty
prettyAffinity (Just affinity) = text "affinity:" <+> (text $ show affinity)

prettyDom :: Integer -> Doc
prettyDom dom = text "dom:" <+> (text $ show dom)

prettyFaultEP :: Maybe Word -> Doc
prettyFaultEP Nothing = empty
prettyFaultEP (Just fault_ep) = text "fault_ep:" <+> (text $ show fault_ep)

prettyExtraInfo :: Maybe TCBExtraInfo -> Doc
prettyExtraInfo Nothing = empty
prettyExtraInfo (Just (TCBExtraInfo addr ip sp elf prio max_prio affinity)) =
    hsep $ punctuate comma $ filter (not . isEmpty)
                   [prettyAddr addr, prettyIP ip, prettySP sp, prettyElf elf, prettyPrio prio, prettyMaxPrio max_prio, prettyAffinity affinity]

prettyInitArguments :: [Word] -> Doc
prettyInitArguments [] = empty
prettyInitArguments init =
    text "init:" <+> brackets (hsep $ punctuate comma $ map num init)

prettyDomainID :: Word -> Doc
prettyDomainID dom = text "domainID:" <+> num dom

prettyPeriod :: Maybe Word64 -> Doc
prettyPeriod Nothing = empty
prettyPeriod (Just period) = text "period:" <+> (text $ show period)

prettyBudget :: Maybe Word64 -> Doc
prettyBudget Nothing = empty
prettyBudget (Just budget) = text "budget:" <+> (text $ show budget)

prettySCData :: Maybe Word -> Doc
prettySCData Nothing = empty
prettySCData (Just scData) = text "data:" <+> (text $ show scData)

prettySCExtraInfo :: Maybe SCExtraInfo -> Doc
prettySCExtraInfo Nothing = empty
prettySCExtraInfo (Just (SCExtraInfo period budget scData)) =
    hsep $ punctuate comma $ filter (not . isEmpty)
              [prettyPeriod period, prettyBudget budget, prettySCData scData]

prettyPCIDevice :: (Word, Word, Word) -> Doc
prettyPCIDevice (pci_bus, pci_dev, pci_fun) =
    num pci_bus <> colon <> num pci_dev <> text "." <> num pci_fun

prettyIOAPICNum :: Word -> Doc
prettyIOAPICNum ioapic = text "ioapic_num:" <+> (text $ show ioapic)

prettyIOAPICPin :: Word -> Doc
prettyIOAPICPin pin = text "ioapic_pin:" <+> (text $ show pin)

prettyIOAPICLevel :: Word -> Doc
prettyIOAPICLevel level = text "ioapic_level:" <+> (text $ show level)

prettyIOAPICPolarity :: Word -> Doc
prettyIOAPICPolarity polarity = text "ioapic_polarity:" <+> (text $ show polarity)

prettyMSIHandle :: Word -> Doc
prettyMSIHandle handle = text "msi_handle:" <+> (text $ show handle)

prettyMSIPCIBus :: Word -> Doc
prettyMSIPCIBus bus = text "msi_pci_bus:" <+> (text $ show bus)

prettyMSIPCIDev :: Word -> Doc
prettyMSIPCIDev dev = text "msi_pci_dev:" <+> (text $ show dev)

prettyMSIPCIFun :: Word -> Doc
prettyMSIPCIFun fun = text "msi_pci_fun:" <+> (text $ show fun)

prettyARMIODevice :: Word -> Doc
prettyARMIODevice iospace = text "iospace:" <+> (text $ show iospace)

prettyFill :: Maybe [String] -> Doc
prettyFill (Just fill) = text "fill:" <+> braces (text $ Data.List.Utils.join " " fill)
prettyFill Nothing = empty

prettyPorts :: (Word, Word) -> Doc
prettyPorts (start, end) =
    text "ports:" <+> brackets (num start <> text ".." <> num end)

prettyObjParams obj = case obj of
    Endpoint -> text "ep"
    Notification -> text "notification"
    TCB _ fault_ep extra dom init ->
        text "tcb" <+> maybeParensList [prettyExtraInfo extra, prettyFaultEP fault_ep, prettyDom dom, prettyInitArguments init]
    CNode _ 0 -> text "irq" --FIXME: This should check if the obj is in the irqNode
    CNode _ bits -> text "cnode" <+> maybeParensList [prettyBits bits]
    Untyped mbits paddr -> text "ut" <+> maybeParensList [prettyMBits mbits, prettyPaddr paddr]

    ASIDPool {} -> text "asid_pool"
    PT {} -> text "pt"
    PD {} -> text "pd"
    PML4 {} -> text "pml4"
    PDPT {} -> text "pdpt"
    Frame vmSz paddr fill -> text "frame" <+> maybeParensList [prettyVMSize vmSz, prettyPaddr paddr, prettyFill fill]

    IOPT _ level -> text "io_pt" <+> maybeParensList [prettyLevel level]
    IOPorts ports -> text "io_ports" <+> maybeParensList [prettyPorts ports]
    IODevice _ dom pci -> text "io_device" <+> maybeParensList [prettyDomainID dom,
                                                                prettyPCIDevice pci]
    ARMIODevice _ iospace -> text "io_device" <+> maybeParensList [prettyARMIODevice iospace]
    VCPU {} -> text "vcpu"
    SC extra mbits -> text "sc" <+> maybeParensList [prettySCExtraInfo extra, prettyMBits mbits]
    RTReply -> text "rtreply"
    IOAPICIrq _ ioapic pin level polarity -> text "ioapic_irq" <+> maybeParensList[prettyIOAPICNum ioapic, prettyIOAPICPin pin, prettyIOAPICLevel level, prettyIOAPICPolarity polarity]
    MSIIrq _ handle bus dev fun -> text "msi_irq" <+> maybeParensList[prettyMSIHandle handle, prettyMSIPCIBus bus, prettyMSIPCIDev dev, prettyMSIPCIFun fun]

capParams [] = empty
capParams xs = parens (hsep $ punctuate comma xs)

successiveWordsUp :: [Maybe Word] -> [Word]
successiveWordsUp [] = []
successiveWordsUp [Just x] = [x]
successiveWordsUp ls@((Just first):(Just second):_)
    | succ first == second = first:(successiveWordsUp (tail ls))
    | otherwise = [first]
successiveWordsUp _ = error "successiveWordsUp"

successiveWordsDown :: [Maybe Word] -> [Word]
successiveWordsDown [] = []
successiveWordsDown [Just x] = [x]
successiveWordsDown ls@((Just first):(Just second):_)
    | first == succ second = first:(successiveWordsDown (tail ls))
    | otherwise = [first]
successiveWordsDown _ = error "successiveWordsDown"

successiveWords :: [Maybe Word] -> [Word]
successiveWords [] = []
successiveWords list = if length up == 1 then down else up
    where up = successiveWordsUp list
          down = successiveWordsDown list

breakSuccessive :: [Maybe Word] -> [[Word]]
breakSuccessive [] = []
breakSuccessive list = range:(breakSuccessive (drop (length range) list))
    where range = successiveWords list

prettyRange :: [Word] -> Doc
prettyRange [x] = num x
prettyRange range =
    num (head range) <> text ".." <> num (last range)

prettyRanges :: [Maybe Word] -> Doc
prettyRanges range =
    hsep $ punctuate comma $ map prettyRange ranges
    where ranges = breakSuccessive range

prettyBrackets :: [Maybe Word] -> Doc
prettyBrackets [Nothing] = empty
prettyBrackets list = brackets (prettyRanges list)

prettyParemNum t n = [text t <> colon <+> num n]

maybeNum _ 0 = []
maybeNum t n = prettyParemNum t n

maybeBadge = maybeNum "badge"

prettyRight _ Read = text "R"
prettyRight _ Write = text "W"
prettyRight True Grant = text "X"
prettyRight False Grant = text "G"

maybeRightsList _ [] = []
maybeRightsList isFrame xs = [hcat (map (prettyRight isFrame) xs)]

maybeRights isFrame r = maybeRightsList isFrame (Set.toList r)

maybeGuard = maybeNum "guard"
maybeGSize = maybeNum "guard_size"

zombieNum n = [text "zombie" <> colon <+> num n]

printAsid (high, low) = text "(" <> num high <> text ", " <> num low <> text ")"

prettyAsid asid = [text "asid:" <+> printAsid asid]

prettyCore core = [text "core:" <+> num core]

maybeAsid Nothing = []
maybeAsid (Just asid) = prettyAsid asid

prettyFrameMapping (container, slot) =
  [text "mapping: (" <> text (showID container) <> text ", " <> num slot <> text ")"]

maybeFrameMapping Nothing = []
maybeFrameMapping (Just mapping) = prettyFrameMapping mapping

maybeCapParams :: Cap -> Doc
maybeCapParams cap = case cap of
    EndpointCap _ badge rights ->
        capParams (maybeBadge badge ++ maybeRights False rights)
    NotificationCap _ badge rights ->
        capParams (maybeBadge badge ++ maybeRights False rights)
    ReplyCap _ -> capParams [text "reply"]
    MasterReplyCap _ -> capParams [text "master_reply"]
    CNodeCap _ guard gsize ->
        capParams (maybeGuard guard ++ maybeGSize gsize)
    FrameCap _ rights asid cached mapping -> capParams (maybeRights True rights ++ maybeAsid asid ++
        (if cached then [] else [text "uncached"]) ++ maybeFrameMapping mapping)
    PTCap _ asid -> capParams (maybeAsid asid)
    PDCap _ asid -> capParams (maybeAsid asid)
    ASIDPoolCap _ asid -> capParams (prettyAsid asid)
    SchedControlCap core -> capParams (prettyCore core)
    _ -> empty

printCap :: Cap -> Doc
printCap cap = case cap of
    NullCap -> text "null"
    IOSpaceMasterCap -> text ioSpaceMaster
    ASIDControlCap -> text asidControl
    IRQControlCap -> text irqControl
    DomainCap -> text domain
    (SchedControlCap {}) -> text schedControl
    _ -> text $ fst $ objID cap

sameName :: ObjID -> ObjID -> Bool
sameName (first, _) (second, _) = first == second

sameParams :: Cap -> Cap -> Bool
sameParams cap1 cap2 =
    case (cap1, cap2) of
    ((EndpointCap _ b1 r1), (EndpointCap _ b2 r2)) -> b1 == b2 && r1 == r2
    ((NotificationCap _ b1 r1), (NotificationCap _ b2 r2)) ->
        b1 == b2 && r1 == r2
    ((CNodeCap _ g1 gs1), (CNodeCap _ g2 gs2)) ->
        g1 == g2 && gs1 == gs2
    ((FrameCap _ r1 a1 c1 m1), (FrameCap _ r2 a2 c2 m2)) -> r1 == r2 && a1 == a2 && c1 == c2 && m1 == m2
    ((PTCap _ a1), (PTCap _ a2)) -> a1 == a2
    ((PDCap _ a1), (PDCap _ a2)) -> a1 == a2
    _ -> True

sameCapName :: Cap -> Cap -> Bool
sameCapName first second
    | not (hasObjID first) || not (hasObjID second) = False
    | snd (objID first) == Nothing || snd (objID second) == Nothing = False
    | otherwise = sameName (objID first) (objID second)

sameCap :: Cap -> Cap -> Bool
sameCap first second =
    sameCapName first second && sameParams first second

class Arrayable a where
    isSameArray :: a -> a -> Bool

instance Arrayable Cap where
    isSameArray = sameCap

instance Arrayable ObjID where
    isSameArray = sameName

sameArray :: (Printing a, Arrayable b) => [(a, b)] -> [(a, b)]
sameArray [] = []
sameArray [x] = [x]
sameArray ls@(x@(slot1, first):(slot2, second):_)
    | isSameArray first second && isSucc slot1 slot2 = x:(sameArray (tail ls))
    | otherwise = [x]

same :: Printing a => (ObjID, KernelObject a) -> (ObjID, KernelObject a) -> Bool
same (name1, obj1) (name2, obj2) =
    if (hasSlots obj1 && hasSlots obj2)
    then sameName name1 name2 && slots obj1 == slots obj2
    else sameName name1 name2

prettyArch ARM11 = text "arm11"
prettyArch IA32  = text "ia32"
prettyArch X86_64 = text "x86_64"

-- Helpers for sorting function
sizeOf :: Arch -> KernelObject Word -> Word
sizeOf _ (Frame vmSz _ _) = vmSz
sizeOf _ (Untyped (Just bSz) _) = 2 ^ bSz
sizeOf IA32 (CNode _ bSz) = 16 * 2 ^ bSz
sizeOf ARM11 (CNode _ bSz) = 16 * 2 ^ bSz
sizeOf X86_64 (CNode _ bSz) = 32 * 2 ^ bSz
sizeOf _ Endpoint = 16
sizeOf IA32 Notification = 16
sizeOf ARM11 Notification = 16
sizeOf X86_64 Notification = 32
sizeOf _ ASIDPool {} = 4 * 2^10
sizeOf _ IOPT {} = 4 * 2^10
sizeOf _ IODevice {} = 1
sizeOf IA32 TCB {} = 2^10
sizeOf IA32 PD {} = 4 * 2^10
sizeOf IA32 PT {} = 4 * 2^10
sizeOf IA32 SC {} = 60
sizeOf ARM11 TCB {} = 512
sizeOf ARM11 PD {} = 16 * 2^10
sizeOf ARM11 PT {} = 2^10
sizeOf ARM11 SC {} = 60
sizeOf ARM11 ARMIODevice {} = 1
sizeOf X86_64 TCB {} = 2^10
sizeOf X86_64 PT {} = 4 * 2^10
sizeOf X86_64 PD {} = 4 * 2^10
sizeOf X86_64 PDPT {} = 4 * 2^10
sizeOf X86_64 PML4 {} = 4 * 2^10
sizeOf X86_64 SC {} = 60
sizeOf _ _ = 0

objPaddr :: KernelObject Word -> Maybe Word
objPaddr (Frame _ paddr _) = paddr
objPaddr (Untyped _ paddr) = paddr
objPaddr _ = Nothing

{- A custom sorting function for CapDL objects. Essentially, we sort by
 - (physical address, descending size, name).
 -
 - We place objects that have physical addresses first. These are almost
 - certainly being allocated from device untypeds and need to be allocated
 - by the capDL loader in physical address order.
 -
 - Other objects will be allocated from normal untypeds and should be in
 - descending order of size to reduce fragmentation. But we also want to give
 - some finer control to the user producing the input specification. For this,
 - we sort objects secondarily by their name. This means the spec creator can
 - name their objects to induce a specific ordering for identically sized
 - objects. This is primarily useful for getting physically contiguous frames.
 -}
sorter :: Arch -> (ObjID, KernelObject Word) -> (ObjID, KernelObject Word) -> Ordering
sorter arch a b =
    if has_paddr a || has_paddr b
        then paddr a `compare` paddr b
        else
            if a_size == b_size
                then fst a `compare` fst b
                else b_size `compare` a_size -- Arguments reversed for largest to smallest
    where
        a_size = sizeOf arch $ snd a
        b_size = sizeOf arch $ snd b
        has_paddr kobj = isJust (objPaddr (snd kobj))
        paddr kobj = fromMaybe 1 (objPaddr (snd kobj))

sortObjects :: Arch -> [(ObjID, KernelObject Word)] -> [(ObjID, KernelObject Word)]
sortObjects arch = sortBy (sorter arch)
