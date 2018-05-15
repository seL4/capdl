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

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module CapDL.MakeModel where

import CapDL.Model
import CapDL.AST
import CapDL.State

import Prelude ()
import Prelude.Compat
import Data.Maybe
import Data.List.Compat
import Data.Either as Either
import Data.Data
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad.State as ST
import Control.Monad

type SlotState = ST.State Word

getSlot :: SlotState Word
getSlot = do
    slot <- ST.get
    return $ slot + 1

putSlot :: Word -> SlotState ()
putSlot = ST.put

isUntyped :: KernelObject Word -> Bool
isUntyped (Untyped {}) = True
isUntyped _ = False

emptyModel arch = Model arch Map.empty Map.empty
emptyIdents = Idents Map.empty

printID :: ObjID -> String
printID (name, Nothing) = name
printID (name, Just num) = name ++ "[" ++ show num ++ "]"

baseName :: QName -> NameRef
baseName = last

qNames :: QName -> [NameRef]
qNames = init

sameName :: ObjID -> ObjID -> Bool
sameName (first, _) (second, _) = first == second

numObject :: Map.Map ObjID a -> Name -> Word
numObject objs name =
    if Map.member (name, Just 0) objs
    then fromIntegral $ length $ filter (sameName (name, Nothing)) $ Map.keys objs
    else error $ "Unknown reference: " ++ name

refToID :: NameRef -> ObjID
refToID (name, []) = (name, Nothing)
refToID (name, [Only n]) = (name, Just n)
refToID names = error ("A unique identifer was expected at " ++ show names)

unrange :: Word -> Range -> [Maybe Word]
unrange num range = case range of
    Only id -> [Just id]
    FromTo first last -> map Just list
        where list = if first <= last
                     then [first..last]
                     else [first,first-1..last]
    From first -> map Just [first..num - 1]
    To last -> map Just [0..last]
    All -> map Just [0..num - 1]

refToIDs :: Map.Map ObjID a -> NameRef -> [ObjID]
refToIDs _ (name, []) = [(name, Nothing)]
refToIDs objs (name, ranges) =
    zip (repeat name) $ concatMap (unrange (numObject objs name)) ranges

makeIDs :: Name -> Maybe Word -> [ObjID]
makeIDs name Nothing = [(name,Nothing)]
makeIDs name (Just num) = zip (repeat name) (map Just [0..(num - 1)])

members :: Ord k => [k] -> Map.Map k a -> Bool
members names objs = all (flip Map.member objs) names

addCovered :: ObjMap Word -> [ObjID] -> ObjSet -> ObjSet
addCovered objs names cov =
    if members names objs
    then foldl' (flip Set.insert) cov names
    else error ("At least one object reference is unknown: " ++ show names)

getUTCov :: CoverMap -> ObjID -> ObjSet
getUTCov covers ut =
    case Map.lookup ut covers of
        Nothing -> Set.empty
        Just cov -> cov

addUTCover :: ObjMap Word -> CoverMap -> [ObjID] -> ObjID -> CoverMap
addUTCover objs covers names ut =
    let cov = getUTCov covers ut
    in Map.insert ut (addCovered objs names cov) covers

addUTCovers :: ObjMap Word -> CoverMap -> [ObjID] -> [ObjID] -> CoverMap
addUTCovers _ covers _ [] = covers
addUTCovers objs covers n [ut] = addUTCover objs covers n ut
addUTCovers objs covers n (ut:uts) =
    addUTCovers objs (addUTCover objs covers n ut) [ut] uts

addUTDecl ::  ObjMap Word -> ObjID -> CoverMap -> NameRef -> CoverMap
addUTDecl objs ut covers names = addUTCover objs covers (refToIDs objs names) ut

addUTDecls ::  ObjMap Word -> ObjID -> CoverMap -> [NameRef] -> CoverMap
addUTDecls objs ut = foldl' (addUTDecl objs ut)

getUntypedCover :: [NameRef] -> ObjMap Word -> CoverMap -> Decl -> CoverMap
getUntypedCover ns objs covers (ObjDecl (KODecl objName obj)) =
    if null (objDecls obj)
    then
        let (name, num) = refToID $ baseName objName
            qns = qNames objName
        in addUTCovers objs covers (makeIDs name num)
                       (map refToID (reverse (ns ++ qns)))
    else
        let name = refToID $ baseName objName
            qns = qNames objName
            covers' = addUTCovers objs covers [name]
                                  (map refToID (reverse (ns ++ qns)))
            covers'' = addUTDecls objs name covers' (Either.rights (objDecls obj))
        in getUntypedCovers (ns ++ objName) objs covers''
                                        (map ObjDecl (lefts (objDecls obj)))
getUntypedCover _ _ covers _ = covers

getUntypedCovers :: [NameRef] -> ObjMap Word -> CoverMap -> [Decl] -> CoverMap
getUntypedCovers ns objs =
    foldl' (getUntypedCover ns objs)

emptyUntyped :: KernelObject Word
emptyUntyped = Untyped Nothing Nothing

getUTObj :: ObjMap Word -> ObjID -> KernelObject Word
getUTObj objs ut =
    case Map.lookup ut objs of
        Nothing -> emptyUntyped
        Just obj ->
            if isUntyped obj
            then obj
            else error ("Untyped object expected at \""++ printID ut ++
                        "\", but found instead: " ++ show obj)

addUTName :: ObjMap Word -> ObjID -> ObjMap Word
addUTName objs ut = Map.insert ut (getUTObj objs ut) objs

addUTNames :: ObjMap Word -> [ObjID] -> ObjMap Word
addUTNames objs [] = objs
addUTNames objs [ut] = addUTName objs ut
addUTNames objs (ut:uts) = addUTNames (addUTName objs ut) uts

addUntyped :: ObjMap Word -> Decl -> ObjMap Word
addUntyped objs (ObjDecl (KODecl objName obj)) =
    if null (objDecls obj)
    then
        let qns = qNames objName
        in addUTNames objs (map refToID qns)
    else
        let qns = qNames objName
            objs' = addUTNames objs (map refToID qns)
        in addUntypeds objs' (map ObjDecl (lefts (objDecls obj)))
addUntyped objs _ = objs

addUntypeds :: ObjMap Word -> [Decl] -> ObjMap Word
addUntypeds = foldl' addUntyped

getTCBAddr :: [ObjParam] -> Maybe Word
getTCBAddr [] = Nothing
getTCBAddr (TCBExtraParam (Addr addr) : _) = Just addr
getTCBAddr (_ : xs ) = getTCBAddr xs

getTCBip :: [ObjParam] -> Maybe Word
getTCBip [] = Nothing
getTCBip (TCBExtraParam (IP ip) : _) = Just ip
getTCBip (_ : xs ) = getTCBip xs

getTCBsp :: [ObjParam] -> Maybe Word
getTCBsp [] = Nothing
getTCBsp (TCBExtraParam (SP sp) : _) = Just sp
getTCBsp (_ : xs ) = getTCBsp xs

getTCBelf :: [ObjParam] -> Maybe String
getTCBelf [] = Nothing
getTCBelf (TCBExtraParam (Elf elf) : _) = Just elf
getTCBelf (_ : xs ) = getTCBelf xs

getTCBprio :: [ObjParam] -> Maybe Integer
getTCBprio [] = Nothing
getTCBprio (TCBExtraParam (Prio prio) : _) = Just prio
getTCBprio (_ : xs) = getTCBprio xs

getTCBmax_prio :: [ObjParam] -> Maybe Integer
getTCBmax_prio [] = Nothing
getTCBmax_prio (TCBExtraParam (MaxPrio max_prio) : _) = Just max_prio
getTCBmax_prio (_ : xs) = getTCBmax_prio xs

getTCBaffinity :: [ObjParam] -> Maybe Integer
getTCBaffinity [] = Nothing
getTCBaffinity (TCBExtraParam (Affinity affinity) : _) = Just affinity
getTCBaffinity (_ : xs) = getTCBaffinity xs

getExtraInfo :: Name -> [ObjParam] -> Maybe TCBExtraInfo
getExtraInfo n params =
    -- FIXME: This is really hacky hardcoding the acceptable combinations of attributes.
    case (getTCBAddr params, getTCBip params, getTCBsp params, getTCBelf params, getTCBprio params, getTCBaffinity params) of
        (Just addr, Just ip, Just sp, Just elf, Just prio, Just affinity) ->
            Just $ TCBExtraInfo addr (Just ip) (Just sp) (Just elf) (Just prio) Nothing (Just affinity)
        (Just addr, Just ip, Just sp, Nothing, Just prio, Just affinity) ->
            Just $ TCBExtraInfo addr (Just ip) (Just sp) Nothing (Just prio) Nothing (Just affinity)
        (Just addr, Nothing, Nothing, Nothing, Nothing, Just affinity) ->
            Just $ TCBExtraInfo addr Nothing Nothing Nothing Nothing Nothing (Just affinity)
        (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) -> Nothing
        params -> error $ "Incorrect extra tcb parameters: " ++ n ++ show params

getTCBDom :: [ObjParam] -> Integer
getTCBDom [] = 0
getTCBDom (Dom dom : _) = dom
getTCBDom (_ : xs) = getTCBDom xs

getFaultEP :: [ObjParam] -> Maybe Word
getFaultEP [] = Nothing
getFaultEP (FaultEP fault_ep : _) = Just fault_ep
getFaultEP (_ : xs) = getFaultEP xs

getInitArguments :: [ObjParam] -> [Word]
getInitArguments [] = []
getInitArguments (InitArguments init : _) = init
getInitArguments (_ : xs) = getInitArguments xs

getSCPeriod :: [ObjParam] -> Maybe Word64
getSCPeriod [] = Nothing
getSCPeriod (SCExtraParam (Period period) : _) = Just period
getSCPeriod (_ : xs) = getSCPeriod xs

getSCBudget :: [ObjParam] -> Maybe Word64
getSCBudget [] = Nothing
getSCBudget (SCExtraParam (Budget budget) : _) = Just budget
getSCBudget (_ : xs) = getSCBudget xs

getSCData :: [ObjParam] -> Maybe Word
getSCData [] = Nothing
getSCData (SCExtraParam (SCData scData) : _) = Just scData
getSCData (_ : xs) = getSCData xs

getSCExtraInfo :: Name -> [ObjParam] -> Maybe SCExtraInfo
getSCExtraInfo n params =
    -- FIXME: This is really hacky hardcoding the acceptable combinations of attributes.
    case (getSCPeriod params, getSCBudget params, getSCData params) of
        (Just period, Just budget, Just scData) ->
            Just $ SCExtraInfo (Just period) (Just budget) (Just scData)
        (Just period, Just budget, Nothing) ->
            Just $ SCExtraInfo (Just period) (Just budget) Nothing
        (Nothing, Nothing, Nothing) -> Nothing
        params -> error $ "Incorrect extra sc parameters: " ++ n ++ show params

getIOAPICIrqIoapic :: [ObjParam] -> Word
getIOAPICIrqIoapic [] = error $ "Incorrect ioapic irq parameters"
getIOAPICIrqIoapic (IOAPICIRQExtraParam (IOAPIC ioapic) : _) = ioapic
getIOAPICIrqIoapic (_ : xs) = getIOAPICIrqIoapic xs

getIOAPICIrqPin :: [ObjParam] -> Word
getIOAPICIrqPin [] = error $ "Incorrect ioapic irq parameters"
getIOAPICIrqPin (IOAPICIRQExtraParam (Pin pin) : _) = pin
getIOAPICIrqPin (_ : xs) = getIOAPICIrqPin xs

getIOAPICIrqLevel :: [ObjParam] -> Word
getIOAPICIrqLevel [] = error $ "Incorrect ioapic irq parameters"
getIOAPICIrqLevel (IOAPICIRQExtraParam (Level level) : _) = level
getIOAPICIrqLevel (_ : xs) = getIOAPICIrqLevel xs

getIOAPICIrqPolarity :: [ObjParam] -> Word
getIOAPICIrqPolarity [] = error $ "Incorrect ioapic irq parameters"
getIOAPICIrqPolarity (IOAPICIRQExtraParam (Polarity polarity) : _) = polarity
getIOAPICIrqPolarity (_ : xs) = getIOAPICIrqPolarity xs

getMSIIrqHandle :: [ObjParam] -> Word
getMSIIrqHandle [] = error $ "Incorrect msi irq parameters"
getMSIIrqHandle (MSIIRQExtraParam (MSIHandle handle) : _) = handle
getMSIIrqHandle (_ : xs) = getMSIIrqHandle xs

getMSIIrqPCIBus :: [ObjParam] -> Word
getMSIIrqPCIBus [] = error $ "Incorrect msi irq parameters"
getMSIIrqPCIBus (MSIIRQExtraParam (MSIPCIBus bus) : _) = bus
getMSIIrqPCIBus (_ : xs) = getMSIIrqPCIBus xs

getMSIIrqPCIDev :: [ObjParam] -> Word
getMSIIrqPCIDev [] = error $ "Incorrect msi irq parameters"
getMSIIrqPCIDev (MSIIRQExtraParam (MSIPCIDev dev) : _) = dev
getMSIIrqPCIDev (_ : xs) = getMSIIrqPCIDev xs

getMSIIrqPCIFun :: [ObjParam] -> Word
getMSIIrqPCIFun [] = error $ "Incorrect msi irq parameters"
getMSIIrqPCIFun (MSIIRQExtraParam (MSIPCIFun fun) : _) = fun
getMSIIrqPCIFun (_ : xs) = getMSIIrqPCIFun xs

getMaybeBitSize :: [ObjParam] -> Maybe Word
getMaybeBitSize [] = Nothing
getMaybeBitSize (BitSize x : _) = Just x
getMaybeBitSize (_ : xs) = getMaybeBitSize xs

getBitSize :: Name -> [ObjParam] -> Word
getBitSize n xs =
    case getMaybeBitSize xs of
        Nothing -> error ("Needs bitsize parameter: " ++ n)
        Just sz -> sz

getVMSize :: Name -> [ObjParam] -> Word
getVMSize n [] = error ("Needs vmsize parameter: " ++ n)
getVMSize n (VMSize 0 : _) = error ("Cannot make a frame object that has size 0: " ++ n)
getVMSize _ (VMSize x : _) = x
getVMSize n (_ : xs) = getVMSize n xs

getMaybePaddr :: [ObjParam] -> Maybe Word
getMaybePaddr [] = Nothing
getMaybePaddr (Paddr x : _) = Just x
getMaybePaddr (_ : xs) = getMaybePaddr xs

getMaybeFill :: [ObjParam] -> Maybe [String]
getMaybeFill [] = Nothing
getMaybeFill (FrameExtraParam (Fill f) : _) = Just f
getMaybeFill(_ : xs) = getMaybeFill xs

getLevel :: Name -> [ObjParam] -> Word
getLevel n [] = error ("Needs level parameter: " ++ n)
getLevel _ (IOPTLevel l : _) = l
getLevel n (_ : xs) = getLevel n xs

getDomainID :: Name -> [ObjParam] -> Word
getDomainID n [] = error ("Needs domainID parameter: " ++ n)
getDomainID _ (DomainID x : _) = x
getDomainID n (_ : xs) = getDomainID n xs

getPCIDevice :: Name -> [ObjParam] -> (Word, Word, Word)
getPCIDevice n [] = error ("Needs pciDevice parameter: " ++ n)
getPCIDevice _ (PCIDevice x : _) = x
getPCIDevice n (_ : xs) = getPCIDevice n xs

getARMIODevice :: Name -> [ObjParam] -> Word
getARMIODevice n [] = error ("Needs iospace parameter: " ++ n)
getARMIODevice _ (ARMIOSpace x : _) = x
getARMIODevice n (_ : xs) = getARMIODevice n xs

getPorts :: Name -> [ObjParam] -> (Word, Word)
getPorts n [] = error ("Needs ports parameter: " ++ n)
getPorts _ (Ports x : _) = x
getPorts n (_ : xs) = getPorts n xs

orderedSubset :: Eq a => [a] -> [a] -> Bool
orderedSubset [] _ = True
orderedSubset _ [] = False
orderedSubset (x:xs) (y:ys)
    | x == y = orderedSubset xs ys
    | otherwise = orderedSubset (x:xs) ys

sortConstrs :: (Data a, Ord a) => a -> a -> Ordering
sortConstrs x y =
  if toConstr x == toConstr y
     then EQ
     else compare x y

subsetConstrs :: (Data a, Ord a) => [a] -> [a] -> Bool
subsetConstrs xs ys = orderedSubset (map toConstr $ sortBy sortConstrs xs)
                                    (map toConstr $ sortBy sortConstrs ys)

containsConstr :: (Data b) => b -> [b] -> Bool
containsConstr x xs = toConstr x `elem` map toConstr xs

numConstrs :: (Data a) => a -> Int
numConstrs x = length $ dataTypeConstrs $ dataTypeOf x

removeConstr :: Data a => a -> [a] -> [a]
removeConstr x xs = filter (\y -> toConstr x /= toConstr y) xs

validObjPars :: KO -> Bool
validObjPars (Obj TCB_T ps []) =
  subsetConstrs ps (replicate (numConstrs (Addr undefined)) (TCBExtraParam undefined)
                    ++ [InitArguments undefined, Dom undefined, FaultEP undefined])
validObjPars (Obj CNode_T ps []) = subsetConstrs ps [BitSize undefined]
validObjPars (Obj Untyped_T ps _) = subsetConstrs ps [BitSize undefined, Paddr undefined]
validObjPars (Obj Frame_T ps []) =
  subsetConstrs ps [VMSize undefined, Paddr undefined, FrameExtraParam undefined] &&
  (not (containsConstr (Paddr undefined) ps) || containsConstr (VMSize undefined) ps)
validObjPars (Obj IOPT_T ps []) = subsetConstrs ps [IOPTLevel undefined]
validObjPars (Obj IOPorts_T ps []) = subsetConstrs ps [Ports undefined]
validObjPars (Obj IODevice_T ps []) = subsetConstrs ps [DomainID undefined, PCIDevice undefined]
validObjPars (Obj ARMIODevice_T ps []) = subsetConstrs ps [ARMIOSpace undefined]
validObjPars (Obj SC_T ps []) =
  subsetConstrs ps ((replicate (numConstrs (Addr undefined)) (SCExtraParam undefined)) ++ [BitSize undefined])
validObjPars (Obj IOAPICIrqSlot_T ps []) =
  subsetConstrs ps (replicate (numConstrs (Addr undefined)) (IOAPICIRQExtraParam undefined))
validObjPars (Obj MSIIrqSlot_T ps []) =
  subsetConstrs ps (replicate (numConstrs (Addr undefined)) (MSIIRQExtraParam undefined))
validObjPars obj = null (params obj)

objectOf :: Name -> KO -> KernelObject Word
objectOf n obj =
    if validObjPars obj
    then case obj of
        Obj Endpoint_T [] [] -> Endpoint
        Obj Notification_T [] [] -> Notification
        Obj TCB_T ps [] ->
            TCB Map.empty (getFaultEP ps) (getExtraInfo n ps) (getTCBDom ps) (getInitArguments ps)
        Obj CNode_T ps [] -> CNode Map.empty (getBitSize n ps)
        Obj Untyped_T ps _ -> Untyped (getMaybeBitSize ps) (getMaybePaddr ps)
        Obj ASIDPool_T _ [] -> ASIDPool Map.empty
        Obj PT_T _ [] -> PT Map.empty
        Obj PD_T _ [] -> PD Map.empty
        Obj PML4_T _ [] -> PML4 Map.empty
        Obj PDPT_T _ [] -> PDPT Map.empty
        Obj Frame_T ps [] -> Frame (getVMSize n ps) (getMaybePaddr ps) (getMaybeFill ps)
        Obj IOPT_T ps [] -> IOPT Map.empty (getLevel n ps)
        Obj IOPorts_T ps [] -> IOPorts (getPorts n ps)
        Obj IODevice_T ps [] -> IODevice Map.empty (getDomainID n ps) (getPCIDevice n ps)
        Obj ARMIODevice_T ps [] -> ARMIODevice Map.empty (getARMIODevice n ps)
        Obj IrqSlot_T [] [] -> CNode Map.empty 0
        Obj IOAPICIrqSlot_T ps [] -> IOAPICIrq Map.empty (getIOAPICIrqIoapic ps) (getIOAPICIrqPin ps) (getIOAPICIrqLevel ps) (getIOAPICIrqPolarity ps)
        Obj MSIIrqSlot_T ps [] -> MSIIrq Map.empty (getMSIIrqHandle ps) (getMSIIrqPCIBus ps) (getMSIIrqPCIDev ps) (getMSIIrqPCIFun ps)
        Obj VCPU_T [] [] -> VCPU
        Obj SC_T ps [] -> SC (getSCExtraInfo n ps) (getMaybeBitSize ps)
        Obj RTReply_T [] [] -> RTReply
        Obj _ _ (_:_) ->
          error $ "Only untyped caps can have objects as content: " ++
                  n ++ " = " ++ show obj
        _ -> error ("Could not convert: " ++ n ++ " = " ++ show obj)
    else error ("Incorrect params for " ++ n)

insertObjects :: [ObjID] -> KernelObject Word -> ObjMap Word -> ObjMap Word
insertObjects ids obj objs = foldl' (\map id->Map.insert id obj map) objs ids

isMember :: Name -> ObjMap Word -> Bool
isMember name objs =
    Map.member (name, Nothing) objs || Map.member (name, Just 0) objs

addObject :: ObjMap Word -> Decl -> ObjMap Word
addObject objs (ObjDecl (KODecl objName obj)) =
    if not $ CapDL.AST.koType obj == Untyped_T && isMember name objs
    then if isMember name objs
        then error ("Duplicate name declaration: " ++ name)
        else let
            objs' = insertObjects (makeIDs name num) (objectOf name obj) objs
        in addObjects objs' (map ObjDecl (lefts (objDecls obj)))
    else objs
    where (name, num) = refToID $ baseName objName
addObject s _ = s

addObjects :: ObjMap Word -> [Decl] -> ObjMap Word
addObjects = foldl' addObject

addIRQ :: IRQMap -> (Word, ObjID) -> IRQMap
addIRQ irqNode (slot, irq) =
    if Map.member slot irqNode
    then error ("IRQ already mapped: " ++ show slot)
    else Map.insert slot irq irqNode

addIRQs :: IRQMap -> [(Word, ObjID)] -> IRQMap
addIRQs = foldl' addIRQ

getSlotIRQs :: ObjMap Word -> CapMapping -> SlotState [(Word, ObjID)]
getSlotIRQs objs (IRQMapping slot nameRef) = do
    slot' <- checkSlot slot
    let irqs = refToIDs objs nameRef
        lastSlot = slot' + fromIntegral (length irqs - 1)
    putSlot lastSlot
    return $ zip [slot'..lastSlot] irqs
getSlotIRQs _ _ = error "not an IRQMapping"

addIRQMapping :: ObjMap Word -> SlotState IRQMap -> CapMapping -> SlotState IRQMap
addIRQMapping objs irqNode cm = do
    slotIRQs <- getSlotIRQs objs cm
    node <- irqNode
    return $ addIRQs node slotIRQs

addIRQMappings :: ObjMap Word -> IRQMap -> [CapMapping] -> SlotState IRQMap
addIRQMappings objs irqNode =
    foldl' (addIRQMapping objs) (return irqNode)

addIRQNode :: ObjMap Word -> IRQMap -> Decl -> IRQMap
addIRQNode objs irqNode (IRQDecl irqs) =
    if Map.null irqNode
    then ST.evalState (addIRQMappings objs irqNode irqs) (-1)
    else error "Duplicate IRQ node declaration"
addIRQNode _ irqNode _ = irqNode

addIRQNodes :: ObjMap Word -> IRQMap -> [Decl] -> IRQMap
addIRQNodes objs = foldl' (addIRQNode objs)

insertMapping :: KernelObject Word -> (Word, Cap) -> KernelObject Word
insertMapping obj (slot, cap) =
    if hasSlots obj
    then let mappings = slots obj
        in if Map.member slot mappings
        then error ("Slot already filled: " ++ show slot)
        else obj {slots = Map.insert slot cap mappings}
    else error ("This object does not support cap mappings: " ++ show obj)

insertMappings :: KernelObject Word -> [(Word, Cap)] -> KernelObject Word
insertMappings = foldl' insertMapping

getBadge :: [CapParam] -> Word
getBadge [] = 0
getBadge (Badge n : _) = n
getBadge (_ : xs) = getBadge xs

getRights :: [CapParam] -> CapRights
getRights [] = Set.empty
getRights (Rights r : ps) = r `Set.union` getRights ps
getRights (_ : ps) = getRights ps

getGuard :: [CapParam] -> Word
getGuard [] = 0
getGuard (Guard n : _) = n
getGuard (_ : ps) = getGuard ps

getGuardSize :: [CapParam] -> Word
getGuardSize [] = 0
getGuardSize (GuardSize n : _) = n
getGuardSize (_ : ps) = getGuardSize ps

getReplys :: [CapParam] -> [CapParam]
getReplys [] = []
getReplys (Reply : ps) = Reply : getReplys ps
getReplys (MasterReply : ps) = MasterReply : getReplys ps
getReplys (_ : ps) = getReplys ps

getMaybeAsid :: [CapParam] -> Maybe Asid
getMaybeAsid [] = Nothing
getMaybeAsid (Asid asid : _) = Just asid
getMaybeAsid (_ : ps) = getMaybeAsid ps

getAsid :: ObjID -> ObjID -> [CapParam] -> Asid
getAsid containerName objRef ps =
    case getMaybeAsid ps of
        Nothing -> error ("Needs asid parameter for cap to " ++ printID objRef ++
                          " in " ++ printID containerName)
        Just asid -> asid

getCached :: [CapParam] -> Bool
getCached [] = True
getCached (Cached c : _) = c
getCached (_ : ps) = getCached ps

getMaybeMapping :: [CapParam] -> Maybe (ObjID, Word)
getMaybeMapping [] = Nothing
getMaybeMapping (FrameMapping c s : _) = Just (refToID c, s)
getMaybeMapping (_ : ps) = getMaybeMapping ps

validCapPars :: KernelObject Word -> [CapParam] -> Bool
validCapPars (Endpoint {}) ps =
    subsetConstrs (removeConstr (Rights undefined) ps) [Badge undefined]
validCapPars (Notification {}) ps =
    subsetConstrs (removeConstr (Rights undefined) ps) [Badge undefined]
validCapPars (TCB {}) ps =
    subsetConstrs ps [Reply, MasterReply] &&
    (not (containsConstr Reply ps) || not (containsConstr MasterReply ps))
validCapPars (CNode {}) ps = subsetConstrs ps [Guard undefined, GuardSize undefined]
validCapPars (Frame {}) ps =
    subsetConstrs (removeConstr (Rights undefined) ps) [Asid undefined, Cached undefined,
                                                        FrameMapping undefined undefined]
validCapPars (PD {}) ps = subsetConstrs ps [Asid undefined]
validCapPars (PT {}) ps = subsetConstrs ps [Asid undefined]
validCapPars (ASIDPool {}) ps = subsetConstrs ps [Asid undefined]
validCapPars _ ps = null ps

objCapOf :: ObjID -> KernelObject Word -> ObjID -> [CapParam] -> Cap
objCapOf containerName obj objRef params =
    if validCapPars obj params
    then case obj of
        Endpoint -> EndpointCap objRef (getBadge params) (getRights params)
        Notification ->
            NotificationCap objRef (getBadge params) (getRights params)
        TCB {} ->
            case getReplys params of
                [] -> TCBCap objRef
                [Reply] -> ReplyCap objRef
                [MasterReply] -> MasterReplyCap objRef
                _ -> error "invalid cap"
        Untyped {} -> UntypedCap objRef
        CNode _ 0 -> IRQHandlerCap objRef --FIXME: This should check if the obj is in the irqNode
        CNode {} -> CNodeCap objRef (getGuard params) (getGuardSize params)
        Frame {} -> FrameCap objRef (getRights params) (getMaybeAsid params) (getCached params)
                                    (getMaybeMapping params)
        PML4 {} -> PML4Cap objRef (getMaybeAsid params)
        PDPT {} -> PDPTCap objRef (getMaybeAsid params)
        PD {} -> PDCap objRef (getMaybeAsid params)
        PT {} -> PTCap objRef (getMaybeAsid params)
        ASIDPool {} -> ASIDPoolCap objRef (getAsid containerName objRef params)
        IOPT {} -> IOPTCap objRef
        IOPorts {} -> IOPortsCap objRef
        IODevice {} -> IOSpaceCap objRef
        ARMIODevice  {} -> ARMIOSpaceCap objRef
        VCPU {} -> VCPUCap objRef
        SC {} -> SCCap objRef
        RTReply {} -> RTReplyCap objRef
        IOAPICIrq {} -> IRQIOAPICHandlerCap objRef
        MSIIrq {} -> IRQMSIHandlerCap objRef
    else error ("Incorrect params for cap to " ++ printID objRef ++ " in " ++
                printID containerName)

capOf :: ObjMap Word -> ObjID -> [CapParam] -> ObjID -> Cap
capOf objs containerName xs id =
    case Map.lookup id objs of
        Nothing ->
            error ("Unknown object \"" ++ printID id ++
                   "\" for cap in " ++ printID containerName)
        Just obj -> objCapOf containerName obj id xs

capsOf :: ObjMap Word -> ObjID -> [ObjID] -> [CapParam] -> [Cap]
capsOf objs name ids xs = map (capOf objs name xs) ids

checkSlot :: Maybe Word -> SlotState Word
checkSlot (Just slot) = return slot
checkSlot Nothing = getSlot

validCapPars' :: Name -> [CapParam] -> Bool
validCapPars' name ps
    | name == schedControl = subsetConstrs ps [Core undefined]
    | otherwise = null ps

getCore :: ObjID -> [CapParam] -> Word
getCore containerName [] =
    error ("Needs core parameter for sched_control cap in " ++ printID containerName)
getCore _ (Core n:_) = n
getCore containerName (_:ps) = getCore containerName ps

capOf' :: ObjID -> Name -> [CapParam] -> Cap
capOf' containerName capName ps
    | capName == ioSpaceMaster = IOSpaceMasterCap
    | capName == asidControl = ASIDControlCap
    | capName == irqControl = IRQControlCap
    | capName == domain = DomainCap
    | capName == schedControl = SchedControlCap (getCore containerName ps)
    | otherwise =
        error ("capOf' called with a name it doesn't know about: " ++ capName)

slotsAndCapsOf :: ObjMap Word-> ObjID -> CapMapping -> SlotState [(Word, Cap)]
slotsAndCapsOf objs objName (CapMapping slot _ nameRef params _)
    | fst nameRef `elem` capStrings && null (snd nameRef) = do
        let (name, []) = nameRef
        if validCapPars' name params
            then do
                slot' <- checkSlot slot
                putSlot slot'
                return [(slot', capOf' objName name params)]
            else error ("Incorrect params for cap to " ++ name ++
                        " in " ++ printID objName)
    | otherwise = do
        slot' <- checkSlot slot
        let caps = capsOf objs objName (refToIDs objs nameRef) params
            lastSlot = slot' + fromIntegral (length caps - 1)
        putSlot lastSlot
        return $ zip [slot'..lastSlot] caps
slotsAndCapsOf _ _ (CopyOf slot _ _ _ _) = do
    slot' <- checkSlot slot
    putSlot slot'
    return [(slot', NullCap)]
slotsAndCapsOf _ _ _ = error "IRQ or ASID mapping"

addMapping :: ObjMap Word -> ObjID -> KernelObject Word -> CapMapping
              -> SlotState (KernelObject Word)
addMapping objs n obj cm = do
    slotCaps <- slotsAndCapsOf objs n cm
    return $ insertMappings obj slotCaps

addMappings :: ObjMap Word -> ObjID -> KernelObject Word -> [CapMapping]
               -> SlotState (KernelObject Word)
addMappings objs n =
    foldM (addMapping objs n)

hasUnnumbered :: [CapMapping] -> Bool
hasUnnumbered [] = False
hasUnnumbered (x:xs) =
    case slot x of
        Nothing -> True
        _ -> hasUnnumbered xs

hasCopy :: [CapMapping] -> Bool
hasCopy [] = False
hasCopy (CopyOf {}:_) = True
hasCopy (_:xs) = hasCopy xs

validMapping :: [CapMapping] -> Bool
validMapping mappings = not $ hasCopy mappings && hasUnnumbered mappings

addCap :: Model Word -> (ObjID, [CapMapping]) -> Model Word
addCap (Model arch objs irqNode cdt untypedCovers) (id, mappings) =
    case Map.lookup id objs of
        Nothing -> error ("Unknown cap container: " ++ printID id)
        Just obj ->
            if validMapping mappings
            then Model arch (Map.insert id mapped objs) irqNode cdt untypedCovers
            else error $ printID id ++
                                " uses both copies of caps and unnumbered slots"
            where mapped = ST.evalState (addMappings objs id obj mappings) (-1)

addCapDecl :: Model Word -> Decl -> Model Word
addCapDecl m@(Model _ objs _ _ _) (CapDecl names mappings) =
    foldl' addCap m (zip (refToIDs objs names) (repeat mappings))
addCapDecl s _ = s

addCapDecls :: [Decl] -> Model Word -> Model Word
addCapDecls decls m = foldl' addCapDecl m decls

insertCapIdentMapping :: ObjID -> Idents CapName -> CapName -> Word
                         -> Idents CapName
insertCapIdentMapping obj (Idents ids) name slot =
    Idents (Map.insert name (obj,slot) ids)

addCapIdentMapping' :: ObjMap Word -> ObjID -> Idents CapName -> Word -> NameRef
                       -> NameRef -> Idents CapName
addCapIdentMapping' m obj ids slot (names, range) ref =
    let len = length $ refToIDs m ref
        names' = case range of
            [] -> if len == 1
                then [(names, Nothing)]
                else error ("Cannot give a unique name to an array of caps: "
                            ++ names)
            [All] -> zip (repeat names) (map Just [0..fromIntegral len - 1])
            _ -> error "invalid range"
        lastSlot = slot + fromIntegral len - 1
        slots = [slot..lastSlot]
    in foldl' (\ids' (name, s) -> insertCapIdentMapping obj ids' name s)
                                                        ids (zip names' slots)

addCapIdentMapping :: ObjMap Word -> ObjID -> Idents CapName -> CapMapping
                      -> Idents CapName
addCapIdentMapping m obj ids (CapMapping (Just slot) (Just names) ref _ _) =
    addCapIdentMapping' m obj ids slot names ref
addCapIdentMapping m obj ids (CopyOf (Just slot) (Just names) ref _ _) =
    addCapIdentMapping' m obj ids slot names ref
addCapIdentMapping _ _ i _ = i

addCapIdentMappings :: ObjMap Word -> NameRef -> Idents CapName -> [CapMapping]
                       -> Idents CapName
addCapIdentMappings m obj =
    foldl' (addCapIdentMapping m (head (refToIDs m obj)))

addCapIdent :: ObjMap Word -> Idents CapName -> Decl -> Idents CapName
addCapIdent _ (Idents ids) (CapNameDecl name target slot) =
    Idents (Map.insert (name, Nothing) (target', slot) ids)
    where
        target' = refToID target
addCapIdent m i (CapDecl obj mappings) = addCapIdentMappings m obj i mappings
addCapIdent _ i _ = i

capIdents :: ObjMap Word -> [Decl] -> Idents CapName
capIdents m = foldl' (addCapIdent m) emptyIdents

-- FIXME: inefficient, use a proper data structure for this
type CapRefMappings = CapRef -> Maybe CapRef

-- Follow a mapping transitively to the end
transMapping :: (Show a, Eq a) => (a -> Maybe a) -> a -> a
transMapping = transMappingE []

-- FIXME: I'm sure there is a library function for this somewhere
transMappingE :: (Show a, Eq a) => [a] -> (a -> Maybe a) -> a -> a
transMappingE seen m x =
    if x `elem` seen
        then error ("Cyclic reference: " ++ show x)
        else
            case m x of
                Nothing -> x
                Just l -> transMappingE (x:seen) m l

funUpd :: Eq a => (a -> b) -> a -> b -> a -> b
funUpd f x y z = if z == x then y else f z

empty :: a -> Maybe b
empty _ = Nothing

addCapCopyRef :: Idents CapName -> ObjID -> CapRefMappings ->
                 CapMapping -> CapRefMappings
addCapCopyRef (Idents ids) obj m (CopyOf (Just slot) _ target _ _) =
    funUpd m (obj, slot) (Map.lookup (refToID target) ids)
addCapCopyRef _ _ m _ = m

addCapCopyRefs :: ObjMap Word -> Idents CapName -> CapRefMappings -> Decl
                  -> CapRefMappings
addCapCopyRefs map ids m (CapDecl objs mappings) =
    foldl' (\m' obj -> foldl' (addCapCopyRef ids obj) m' mappings)
                                                          m (refToIDs map objs)
addCapCopyRefs _ _ m _ = m

capCopyGraph :: ObjMap Word -> Idents CapName -> [Decl] -> CapRefMappings
capCopyGraph m ids = foldl' (addCapCopyRefs m ids) empty

getMasked :: [CapParam] -> CapRights
getMasked [] = allRights
getMasked (Masked r : ps) = Set.intersection r (getMasked ps)
getMasked (_ : ps) = getMasked ps

copyCapParams :: [CapParam] -> Cap -> Cap
copyCapParams params cap
    | hasRights cap = cap {capRights = Set.intersection rights $ capRights cap}
    | otherwise = cap
    where rights = getMasked params

getSrcCap :: Idents CapName -> CapRefMappings -> Model Word -> ObjID -> Cap
getSrcCap (Idents ids) refs m src =
    case Map.lookup src ids of
        Nothing -> error ("Unknown cap reference: " ++ printID src)
        Just srcRef ->
            case maybeSlotCap (transMapping refs srcRef) m of
                Nothing -> error ("Could not resolve cap reference: "++
                                  show (transMapping refs srcRef))
                Just cap -> cap

addCapCopy :: Idents CapName -> CapRefMappings -> ObjID ->
              Model Word -> CapMapping -> Model Word
addCapCopy ids refs obj m (CopyOf (Just slot) _ src params _) =
    let caps = map (getSrcCap ids refs m) (refToIDs (cap_ids ids) src)
        slots = [slot..(slot + fromIntegral (length caps))]
        caps' = map (copyCapParams params) caps
    in foldl' (\m' (cap, slot') -> ST.execState (setCap (obj,slot') cap) m')
              m (zip caps' slots)
addCapCopy _ _ _ m _ = m

-- FIXME: this recursion pattern over all mappings is duplicated all over
-- the place. factor out.
addCapCopies :: Idents CapName -> CapRefMappings -> NameRef ->
              Model Word -> CapMapping -> Model Word
addCapCopies ids refs names m@(Model _ map _ _ _) copy =
    foldl' (\model obj -> addCapCopy ids refs obj model copy) m
          (refToIDs map names)

addCapCopyDecl :: Idents CapName -> CapRefMappings -> Model Word -> Decl ->
                  Model Word
addCapCopyDecl ids refs m (CapDecl obj mappings) =
    foldl' (addCapCopies ids refs obj) m mappings
addCapCopyDecl _ _ m _ = m

addCapCopyDecls :: Idents CapName -> CapRefMappings -> Model Word -> [Decl] ->
                   Model Word
addCapCopyDecls ids refs = foldl' (addCapCopyDecl ids refs)

getCapCopy :: Idents CapName -> ObjMap Word -> ObjID -> CopyMap ->
              CapMapping -> CopyMap
getCapCopy ids _ obj copies (CopyOf (Just slot) _ src _ _) =
    let capNames = refToIDs (cap_ids ids) src
        capRefs = zip (repeat obj) [slot..(slot + fromIntegral (length capNames))]
    in foldl' (\copies' (capRef, capName) -> Map.insert capRef capName copies')
                                                   copies (zip capRefs capNames)
getCapCopy _ _ _ m _ = m

getCapCopies :: Idents CapName -> ObjMap Word -> NameRef -> CopyMap ->
                CapMapping -> CopyMap
getCapCopies ids m names copies copy =
    foldl' (\copies obj -> getCapCopy ids m obj copies copy) copies
          (refToIDs m names)

getCapCopyDecl :: Idents CapName -> ObjMap Word -> CopyMap -> Decl -> CopyMap
getCapCopyDecl ids m copies (CapDecl obj mappings) =
    foldl' (getCapCopies ids m obj) copies mappings
getCapCopyDecl _ _ copies _ = copies

getCapCopyDecls :: Idents CapName -> ObjMap Word -> [Decl] -> CopyMap
getCapCopyDecls ids m = foldl' (getCapCopyDecl ids m) Map.empty

slotRefToCapRef :: Idents CapName -> SlotRef -> CapRef
slotRefToCapRef _ (Left (obj, slot)) = (refToID obj, slot)
slotRefToCapRef ids (Right name) =
    case Map.lookup (refToID name) (cap_ids ids) of
        Just capRef -> capRef
        Nothing -> error $ "Unknown cap reference: " ++ printID (refToID name)

insertCDT :: CapRef -> CapRef -> CDT -> CDT
insertCDT child parent cdt =
    if isNothing (Map.lookup child cdt)
    then Map.insert child parent cdt
    else error $ show child ++ " has multiple parents"

getDeclOrSlotRef :: Idents CapName -> CapRef -> CDT -> Either Decl SlotRef -> CDT
getDeclOrSlotRef ids parent cdt (Left decl@(CDTDecl child _)) =
    let child' = slotRefToCapRef ids child
        cdt' = insertCDT child' parent cdt
    in getCDTDecl ids cdt' decl
getDeclOrSlotRef _ _ _ (Left _) = error "not a CDTDecl"
getDeclOrSlotRef ids parent cdt (Right child) =
    let child' = slotRefToCapRef ids child
    in insertCDT child' parent cdt

getCDTDecl :: Idents CapName -> CDT -> Decl -> CDT
getCDTDecl ids cdt (CDTDecl parent children) =
    foldl' (getDeclOrSlotRef ids parent') cdt children
    where parent' = slotRefToCapRef ids parent
getCDTDecl _ cdt _ = cdt

getCDTDecls :: Idents CapName -> [Decl] -> CDT
getCDTDecls ids = foldl' (getCDTDecl ids) Map.empty

addCDTMapping :: Idents CapName -> ObjID -> CDT -> CapMapping -> SlotState CDT
addCDTMapping ids obj cdt mapping
    | isJust (maybeParent mapping) = do
        slot' <- checkSlot (slot mapping)
        putSlot slot'
        let parent = slotRefToCapRef ids $ fromJust $ maybeParent mapping
            child = (obj, slot')
        return $ insertCDT child parent cdt
    | otherwise = return cdt

addCDTMappings :: Idents CapName -> CDT -> (ObjID, [CapMapping]) -> SlotState CDT
addCDTMappings ids cdt (obj, mappings) =
    foldM (addCDTMapping ids obj) cdt mappings

addCDTCapDecl :: ObjMap Word -> Idents CapName -> CDT -> Decl -> CDT
addCDTCapDecl objs ids cdt (CapDecl names mappings) =
    foldl' (\cdt capDecl -> ST.evalState (addCDTMappings ids cdt capDecl) (-1))
           cdt (zip (refToIDs objs names) (repeat mappings))
addCDTCapDecl _ _ cdt _ = cdt

addCDTCapDecls :: ObjMap Word -> Idents CapName -> CDT -> [Decl] -> CDT
addCDTCapDecls objs ids = foldl' (addCDTCapDecl objs ids)

makeModel :: Module -> (Model Word, Idents CapName, CopyMap)
makeModel (Module arch decls) =
    let objs = addObjects Map.empty decls
        objs' = addUntypeds objs decls
        irqs = addIRQNodes objs' Map.empty decls
        ids = capIdents objs' decls
        refs = capCopyGraph objs' ids decls
        copies = getCapCopyDecls ids objs' decls
        covers = getUntypedCovers [] objs' Map.empty decls
        cdt = getCDTDecls ids decls
        cdt' = addCDTCapDecls objs' ids cdt decls
    in (flip (addCapCopyDecls ids refs) decls .
        addCapDecls decls $ Model arch objs' irqs cdt' covers, ids, copies)
