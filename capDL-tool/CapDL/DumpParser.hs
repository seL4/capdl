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

module CapDL.DumpParser where

import CapDL.AST
import CapDL.Model
import CapDL.ParserUtils

import Text.ParserCombinators.Parsec

import Prelude ()
import Prelude.Compat
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Compat
import Data.List.Split
import Data.Ord
import Numeric
import Data.Maybe

--Assumes that untypeds come before anything with an address

insertCov :: ObjID -> ObjID -> MapParser ()
insertCov cov untyp = do
    maps <- getState
    let covM = covMap maps
        covSet = case Map.lookup untyp covM of
                     Just set -> set
                     Nothing -> Set.empty
        covSet' = Set.insert cov covSet
    setState $ maps { covMap = Map.insert untyp covSet' covM }

getCovs :: MapParser [(ObjID, Set.Set ObjID)]
getCovs = do
    maps <- getState
    return $ Map.toList $ covMap maps

insertAddrs :: (Word, Word) -> ObjID -> MapParser ()
insertAddrs addrs objID = do
    maps <- getState
    let addrM = addrMap maps
    setState $ maps { addrMap = Map.insert objID addrs addrM }

included :: Word -> (Word, Word) -> Bool
included n (st, end) = n >= st && n < end

getUntypSize :: String -> Int
getUntypSize string = read $ splitOn "@" string !! 2

lookupAddr :: Word -> MapParser (Maybe ObjID)
lookupAddr addr = do
    maps <- getState
    let ids = map fst $ filter (included addr . snd) $ Map.toList $ addrMap maps
        id = minimumBy (comparing (getUntypSize . fst)) ids
    return $ case ids of
                 [] -> Nothing
                 _ -> Just id

insertRef :: Name -> ObjID -> MapParser ()
insertRef dumpID objID = do
    maps <- getState
    let refM = refMap maps
    setState $ maps { refMap = Map.insert dumpID objID refM }

lookupRef :: Name -> MapParser ObjID
lookupRef dumpID = do
    maps <- getState
    let id = Map.lookup dumpID $ refMap maps
    case id of
        Just id -> return id
        Nothing -> return (dumpID, Nothing)

getAddr :: String -> Maybe Word
getAddr string =
    if '@' `elem` string
    then Just $ fst.head.readHex $ splitOn "@0x" string !! 1
    else Nothing

getObjTyp :: String -> Maybe String
getObjTyp string =
    if '@' `elem` string
    then Just $ splitOn "@" string !! 0
    else Nothing

maybeInsertIRQ :: Name -> MapParser ()
maybeInsertIRQ obj = do
    if getObjTyp obj == Just "irqhandler"
     then do
        maps <- getState
        let irqM = irqMap maps
            slot = fromJust $ getAddr obj
        setState $ maps { irqMap = Map.insert slot obj irqM }
     else return ()

object :: MapParser KO
object = do
    typ <- object_type
    params <- object_params
    return (Obj typ params [])

maybe_object :: MapParser (Maybe (Name, KO))
maybe_object =
        do name <- name
           _ <- symbol "="
           obj <- CapDL.DumpParser.object
           return $ Just (name, obj)
    <|> return Nothing

sizeOf :: Arch -> KO -> Word
sizeOf _ (Obj Frame_T [VMSize vmSz] _) = vmSz
sizeOf _ (Obj Untyped_T [BitSize bSz] _) = 2 ^ bSz
sizeOf _ (Obj CNode_T [BitSize bSz] _) = 16 * 2 ^ bSz
sizeOf _ (Obj IrqSlot_T _ _) = 1
sizeOf _ (Obj Endpoint_T _ _) = 16
sizeOf _ (Obj Notification_T _ _) = 16
sizeOf _ (Obj ASIDPool_T _ _) = 4 * 2^10
sizeOf _ (Obj IOPT_T _ _) = 4 * 2^10
sizeOf _ (Obj IODevice_T _ _) = 1
sizeOf _ (Obj ARMIODevice_T _ _) = 1
sizeOf IA32 (Obj TCB_T _ _) = 2^10
sizeOf IA32 (Obj PD_T _ _) = 4 * 2^10
sizeOf IA32 (Obj PT_T _ _) = 4 * 2^10
sizeOf ARM11 (Obj TCB_T _ _) = 512
sizeOf ARM11 (Obj PD_T _ _) = 16 * 2^10
sizeOf ARM11 (Obj PT_T _ _) = 2^10
sizeOf _ _ = 0

consecutive :: Arch -> (Name, KO) -> Maybe (Name, KO) -> Word -> Bool
consecutive _ _ Nothing _ = False
consecutive arch (name1, obj1) (Just (name2, obj2)) num =
    let addr1 = getAddr name1
        addr2 = getAddr name2
    in case (addr1, addr2) of
        (Just addr1, Just addr2) ->
            if obj1 == obj2 && addr1 + num * sizeOf arch obj1 == addr2
            then True
            else False
        _ -> False

considerUntypeds :: Arch -> ObjID -> Maybe Word -> KO -> MapParser ()
considerUntypeds arch refr addr obj = do
    case (addr, obj) of
        (Just addr, Obj Untyped_T _ _ ) -> do
                                    covUn <- lookupAddr addr
                                    insertAddrs (addr, addr + sizeOf arch obj - 1) refr
                                    case covUn of
                                        Just covUn -> insertCov refr covUn
                                        Nothing -> return ()
        (Just addr, _) -> do covUn <- lookupAddr addr
                             case covUn of
                                Just covUn -> insertCov refr covUn
                                Nothing -> return ()
        (Nothing, _) -> return ()

maybe_obj_decl :: Arch -> (Name, KO) -> Word -> MapParser Word
maybe_obj_decl arch pre num = do
        next <- lookAhead maybe_object
        if consecutive arch pre next num
         then do let (name, obj) = fromJust next
                     refr = (fst pre, Just num)
                     addr = getAddr name
                 _ <- maybe_object
                 maybeInsertIRQ name
                 insertRef name refr
                 considerUntypeds arch refr addr obj
                 total <- maybe_obj_decl arch pre (num + 1)
                 return total
         else return num

obj_decl :: Arch -> MapParser KODecl
obj_decl arch = do
    name <- name
    _ <- symbol "="
    obj <- CapDL.DumpParser.object
    total <- maybe_obj_decl arch (name, obj) 1
    let (decl, refr) = if total == 1
                       then ((name, []), (name, Nothing))
                       else ((name, [Only total]), (name, Just 0))
        addr = getAddr name
    maybeInsertIRQ name
    insertRef name refr
    considerUntypeds arch refr addr obj
    return (KODecl [decl] obj)

obj_decls :: Arch -> MapParser [Decl]
obj_decls arch = do
    reserved "objects"
    decls <- braces $ many (obj_decl arch)
    return $ map ObjDecl decls

id_to_ref :: ObjID -> NameRef
id_to_ref (name, Nothing) = (name, [])
id_to_ref (name, Just n) = (name, [Only n])

make_obj :: Set.Set ObjID -> KO
make_obj covs = Obj Untyped_T [] $ map (Right . id_to_ref) $ Set.toList covs

cov_decl :: (ObjID, Set.Set ObjID) -> KODecl
cov_decl (refr, covs) =
    let obj = make_obj covs
    in KODecl [id_to_ref refr] obj

cov_decls :: MapParser [Decl]
cov_decls = do
    covs <- getCovs
    return $ map (ObjDecl . cov_decl) covs

cap_mapping :: MapParser CapMapping
cap_mapping = do
    sl <- maybe_slot
    obj <- name
    (n,num) <- lookupRef obj
    let obj' = case num of
            Just num -> (n, [Only num])
            Nothing -> (n, [])
    params <- cap_params
    parent <- maybe_parent
    return $ CapMapping sl Nothing obj' params parent

refToNameRef :: ObjID -> NameRef
refToNameRef (id, num) =
    case num of
        Just num -> (id, [Only num])
        Nothing -> (id, [])

cap_decl :: MapParser Decl
cap_decl = do
    n <- name
    ref <- lookupRef n
    let n' = refToNameRef ref
    ms <- braces (sepEndBy cap_mapping opt_semi)
    return $ CapDecl n' ms

cap_decls :: MapParser [Decl]
cap_decls = do
    reserved "caps"
    braces $ many (try cap_decl)

make_irq_mapping :: (Word, Name) -> MapParser CapMapping
make_irq_mapping (slot, irq) = do
    ref <- lookupRef irq
    let irq' = refToNameRef ref
    return $ IRQMapping (Just slot) irq'

make_irq_decl :: MapParser [Decl]
make_irq_decl = do
    maps <- getState
    let irqs = Map.toList $ irqMap maps
    irq_decls <- mapM make_irq_mapping irqs
    return [IRQDecl irq_decls]

all_decls :: Arch -> MapParser [Decl]
all_decls arch = do
    objs <- obj_decls arch
    cov <- cov_decls
    caps <- cap_decls
    cdt <- cdt_decls
    irq <- make_irq_decl
    return $ objs ++ cov ++ caps ++ cdt ++ irq

capDLDumpModule :: MapParser Module
capDLDumpModule = do
    whiteSpace
    arch <- parse_arch
    decls <- all_decls arch
    eof
    return (Module arch decls)
