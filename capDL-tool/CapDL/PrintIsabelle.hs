--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

module CapDL.PrintIsabelle where

import CapDL.Model
import CapDL.State (koType, lookupSizeMap, objSizeBits)
import CapDL.PrintUtils (printAsid)

import Prelude ()
import Prelude.Compat hiding ((<>))
import Text.PrettyPrint
import Numeric (showHex)
import Data.List.Compat
import Data.Ord (comparing)
import Control.Monad (unless)
import Control.Monad.State.Strict (StateT (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Bits
import System.FilePath.Posix
import Text.Regex (mkRegex, matchRegex)

-- This lifts `mapAccumL` over a monad
mapAccumM :: (Monad m, Traversable t) =>
             (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f z xs = swap <$> runStateT (traverse (StateT . f') xs) z
  where swap (a, b) = (b, a)
        f' x z = swap <$> f z x

lookupElem :: (Eq a) => a -> Map.Map k a -> Maybe k
lookupElem x m =
    case find ((==) x . snd) (Map.toList m) of
        Just (k, _) -> Just k
        Nothing -> Nothing

mapElem :: (Eq a) => a -> Map.Map k a -> Bool
mapElem x m = case lookupElem x m of
        Just _ -> True
        Nothing -> False

indent = 2

num n = int (fromIntegral n)

hex :: Word -> String
hex x = "0x" ++ showHex x ""

equiv = text "\\<equiv>"

lambda = text "\\<lambda>"

isaComment x = "\\<comment> \\<open>" ++ x ++ "\\<close>"

{- This mangles hierarchical names into valid Isabelle identifiers:
 - we replace '.' by '\'' and check that the result would be valid. -}
mangle :: String -> String
mangle = fixDots . checkValid
  where -- Technically, we could support more names, but we don't
        -- expect the camkes toolchain to generate them.
        nameRE = mkRegex "^[a-zA-Z]([a-zA-Z0-9_.]*[a-zA-Z0-9])?$"
        checkValid s = case matchRegex nameRE s of
                           Just _ -> s
                           _ -> error $ "can't mangle object name to Isabelle: " ++ show s
        fixDots = map (\c -> if c == '.' then '\'' else c)

showID :: ObjID -> String
showID (name, Nothing) = mangle name
showID (name, Just num) = mangle $ name ++ "_" ++ show num

capsName id = showID id ++ "_caps"

constdefs :: String -> String -> Doc
constdefs name typ =
    text ("definition " ++ name ++ " :: ") <> doubleQuotes (text typ) <+>
    text "where"

{- Prove a set of lemmas. The given proof script is assumed to
 - discharge all the proof goals. -}
lemmas :: String -> [String] -> [String] -> Doc
lemmas name statements proof =
  text ("lemma " ++ name ++ ": ") $+$
  nest 2 (vcat (map (doubleQuotes . text) statements)) $+$
  nest 2 (vcat (map text proof))

-- Prove a single lemma.
lemma :: String -> String -> [String] -> Doc
lemma name statement proof =
  lemmas name [statement] proof

record :: Doc -> Doc
record p = text "\\<lparr>" <+> p <+> text "\\<rparr>"

printSet :: [Doc] -> Doc
printSet ls = braces $ hsep $ punctuate comma ls

filterEmpty :: [Doc] -> [Doc]
filterEmpty = filter (not . isEmpty)

-- Untypeds are not kernel objects in the Isabelle models.
notUntyped :: KernelObject a -> Bool
notUntyped Untyped{} = False
notUntyped _ = True

{- HACK: fake location for IRQ nodes.
 - IRQ CNodes are model-level objects, so we need to invent a physical
 - presence for them even if the capDL input doesn't mention them.
 - Suitably aligned for 1024 irqs. DSpec probably doesn't care, though. -}
irq_node_paddr__hack :: Word
irq_node_paddr__hack = 0x10000

{-
 - Helper that recovers memory layout from a single untypedCover.
 -}

-- Check that we allocate objects consistently with their paddrs.
checkEqPaddr :: KernelObject a -> Word -> Bool
checkEqPaddr obj p = maybe True (p ==) (objPaddr obj)

untypedCoverToPaddrs ::
    ObjectSizeMap -> ObjMap Word -> ObjID -> KernelObject Word -> [ObjID]
    -> Either (String, [ObjID]) (Map.Map ObjID Word)
untypedCoverToPaddrs sizeMap objs
      utID (Untyped { maybePaddr = Just utPaddr, maybeSizeBits = Just utSizeBits })
      cover =
    do
       let getObject id =
               case Map.lookup id objs of
                   Just o -> return o
                   _      -> Left ("Object in untyped but not in objects list", [id, utID])
           utSize = 2^utSizeBits
           utEnd = utPaddr + utSize
           alloc1 addr objID =
               do obj <- getObject objID
                  let objSize = 2^objSizeBits sizeMap obj
                  unless (checkEqPaddr obj addr) $
                      Left ("Object addr doesn't match untyped's current addr (" ++
                            hex addr ++ ")", [utID, objID])
                  let addr' = addr + objSize
                  unless (addr `mod` objSize == 0) $
                      Left ("Can't place obj at unaligned addr (addr=" ++
                            hex addr ++ ", obj size=" ++
                            hex objSize ++ ")", [utID, objID])
                  unless (addr' <= utEnd) $
                      Left ("Can't place obj at addr outside untyped (addr=" ++
                            hex addr ++ ", ut end=" ++ hex utEnd ++ ", obj size=" ++
                            hex objSize ++ ")", [utID, objID])
                  return (addr', (objID, addr))
       Map.fromList . snd <$> mapAccumM alloc1 utPaddr cover
-- unused, or child untyped; no-op
untypedCoverToPaddrs _ _ _ Untyped{} [] = return Map.empty
untypedCoverToPaddrs _ _ utID Untyped{} (:){} =
    Left ("Untyped has children but is missing paddr or size", [utID])
untypedCoverToPaddrs _ _ koID _ _ =
    error $ "internal error in untypedCoverToPaddrs: not an untyped: " ++ show koID

{-
 - Determine the memory address (i.e. cdl_object_id) for each object.
 - This only works for capDL specs that have been pre-allocated, i.e.,
 - every object belongs to an untypedCover that has a physical address.
 -}
getPhysAddrs :: ObjectSizeMap -> ObjMap Word -> CoverMap
                -> Either (String, [ObjID]) (Map.Map ObjID Word)
getPhysAddrs objSizeMap objs untypedCovers =
    Map.unions <$>
    sequence [
        Map.insert utID utPaddr <$> -- add untyped's own addr
            untypedCoverToPaddrs objSizeMap objs utID ut cover
        | (utID, cover) <- Map.toList untypedCovers,
          let ut@Untyped{ maybePaddr = Just utPaddr } = objs Map.! utID ]

{-
 - Look up object locations that have been computed by getPhysAddrs.
 - Also invent locations for IRQ nodes.
 -}
getAddr :: ObjectSizeMap -> Map.Map ObjID Word -> IRQMap -> ObjID -> Word
getAddr objSizeMap objAddrs irqNode id
    | Just irq <- lookupElem id irqNode = irq_node_paddr__hack + irq * slotSize
    | Just addr <- Map.lookup id objAddrs = addr
    | otherwise = error $ "getID: no address for object: " ++ showID id
    where slotSize = 2^lookupSizeMap CNode_T objSizeMap

getID :: ObjectSizeMap -> Map.Map ObjID Word -> IRQMap -> ObjID -> Doc
getID objSizeMap objAddrs irqNode id = text $ hex $ getAddr objSizeMap objAddrs irqNode id

printID :: ObjID -> Doc
printID id = text (showID id ++ "_id")

printRight :: Rights -> Doc
printRight = text . show

printRightsList :: [Rights] -> [Doc]
printRightsList [] = [empty]
printRightsList xs = map printRight xs

printRights :: CapRights -> Doc
printRights r =
    printSet $ printRightsList $ Set.toList r

printFrameSize :: ObjID -> ObjMap Word -> Doc
printFrameSize id ms =
    let Just (Frame sz _ _) = Map.lookup id ms
    in num sz

printCoverSet :: ObjMap Word -> Maybe [ObjID] -> Doc
printCoverSet _ Nothing = printSet []
printCoverSet ms (Just objs) = printSet $ map printID objs'
    where objs' = filter (\id -> isJust $ Map.lookup id ms) objs

printMaybeAsid :: Maybe Asid -> Doc
printMaybeAsid Nothing = text "None"
printMaybeAsid (Just asid) = parens $ text "Some" <+> printAsid asid

printReal :: Bool -> Doc
printReal True = text "Real"
printReal False = text "Fake"

printCNodeSize :: ObjMap Word -> ObjID -> Doc
printCNodeSize ms id =
    let (CNode _ sz) = fromJust $ Map.lookup id ms
    in num sz

-- FIXME: missing capDL attribute
untypedIsDev :: KernelObject Word -> Bool
untypedIsDev Untyped{} = False -- lies
untypedIsDev obj = error $ "untypedIsDev: got " ++ show (koType obj)

-- The bool represents whether the cap is a real cap;
-- i.e. if it is not in a PT or PD
printCap :: ObjMap Word -> IRQMap -> CoverMap -> Bool -> Cap -> Doc
printCap _ _ _ _ NullCap = text "NullCap"
printCap ms _ _ _ (UntypedCap id) =
    text "UntypedCap" <+> text (show $ untypedIsDev (ms Map.! id)) <+>
    parens (text "ptr_range" <+> printID id <+> num utSize) <+>
    printSet [] -- TODO: check implied free range (or assert ut is empty)
    where Just Untyped{ maybeSizeBits = Just utSize } =
              Map.lookup id ms
printCap _ _ _ _ (EndpointCap id badge rights) = text "EndpointCap" <+>
    printID id <+> num badge <+> printRights rights
printCap _ _ _ _ (NotificationCap id badge rights) = text "NotificationCap" <+>
    printID id <+> num badge <+> printRights rights
printCap _ _ _ _ (ReplyCap id) = text "ReplyCap" <+> printID id
printCap _ _ _ _ (MasterReplyCap id) =
    text "MasterReplyCap" <+>  printID id
printCap ms _ _ _ (CNodeCap id guard gsize) =
    text "CNodeCap" <+>  printID id <+> num guard <+> num gsize <+>
    printCNodeSize ms id
printCap _ _ _ _ (TCBCap id) = text "TcbCap" <+> printID id
printCap _ _ _ _ IRQControlCap = text "IrqControlCap"
printCap _ irqNode _ _ (IRQHandlerCap id) =
    text "IrqHandlerCap" <+> num (fromJust (lookupElem id irqNode))
printCap _ _ _ _ DomainCap = text "DomainCap"
printCap ms _ _ real (FrameCap id rights asid cached _) = text "FrameCap" <+>
    -- is_device flag, assumed always false. FIXME: add to model?
    text "False" <+>
    printID id <+> printRights rights <+> printFrameSize id ms <+>
    printReal real <+> printMaybeAsid asid <+>
    text (if cached then "" else isaComment "uncached")
printCap _ _ _ real (PTCap id asid) =
    text "PageTableCap" <+> printID id <+> printReal real <+>
    printMaybeAsid asid
printCap _ _ _ real  (PDCap id asid) =
    text "PageDirectoryCap" <+> printID id <+> printReal real <+>
    printMaybeAsid asid
printCap _ _ _ _ ASIDControlCap = text "AsidControlCap"
printCap ms _ _ _ (ASIDPoolCap id) =
    text "AsidPoolCap" <+> printID id <+> asidHigh
    -- In capDL, asid_high is a property of the asid_pool (as it should be),
    -- but in DSpec it is a property of ASIDPoolCaps. We do the translation here.
    where asidHigh =
              case Map.lookup id ms of
                  Just (ASIDPool _ (Just asidHigh)) -> text (hex asidHigh)
                  _ -> text "undefined"
printCap _ _ _ _ cap = text $ "undefined " ++ isaComment ("unsupported cap: " ++ show cap)

printCapMapping :: ObjMap Word -> IRQMap -> CoverMap -> Bool -> (Word, Cap) -> Doc
printCapMapping ms irqNode covers real (slot, cap) =
    num slot <+> text "\\<mapsto>"
                      <+> text "Types_D." <> printCap ms irqNode covers real cap

printCapMap :: ObjMap Word -> IRQMap -> CoverMap -> Bool -> CapMap Word -> Doc
printCapMap ms irqNode covers real slots =
    case map (printCapMapping ms irqNode covers real) (Map.toList slots) of
        [] -> text "Map.empty"
        xs -> brackets $ vcat $ punctuate comma xs

printCaps' :: ObjMap Word -> ObjID -> IRQMap -> CoverMap -> Bool -> CapMap Word -> Doc
printCaps' ms id irqNode covers real slots = constdefs name "cdl_cap_map" $+$
    doubleQuotes (text name <+> equiv <+> printCapMap ms irqNode covers real slots)
    where name = capsName id

printCaps :: ObjMap Word -> ObjID -> IRQMap -> CoverMap -> KernelObject Word -> Doc
printCaps ms id irqNode covers (PT slots) =
                          printCaps' ms id irqNode covers False slots $+$ text ""
printCaps ms id irqNode covers (PD slots) =
                          printCaps' ms id irqNode covers False slots $+$ text ""
printCaps ms id irqNode covers (ASIDPool slots _) =
                          printCaps' ms id irqNode covers False slots $+$ text ""
printCaps ms id irqNode covers obj
    | hasSlots obj = printCaps' ms id irqNode covers True (slots obj) $+$ text ""
    | otherwise = empty

getAddress' :: ObjMap Word -> [Cap] -> Word -> Cap -> (Word, Cap) -> Maybe Word
getAddress' ms seen current_word goal_cap (slot, cap) =
    getAddress ms seen (current_word + slot) goal_cap cap

getAddress :: ObjMap Word -> [Cap] -> Word -> Cap -> Cap -> Maybe Word
getAddress ms seen current_word goal_cap cap@(CNodeCap objID guard gsz)
    | cap `elem` seen = Nothing
    | otherwise =
        let (CNode slots sz) = fromJust $ Map.lookup objID ms
            radix_sz = fromIntegral sz
            level_sz = fromIntegral $ gsz + sz
            new_word = shift current_word level_sz + shift guard radix_sz
        in case lookupElem goal_cap slots of
                Just slot -> Just (new_word + slot)
                Nothing -> listToMaybe $ mapMaybe
                                 (getAddress' ms (cap:seen) new_word goal_cap)
                                 (Map.toList slots)
getAddress _ _ _ _ _ = Nothing

hasFaultEndpoint :: Maybe Word -> String
hasFaultEndpoint fault =
    case fault of
        Just _ -> "True"
        Nothing -> "False"

printObjID :: ObjectSizeMap -> Map.Map ObjID Word -> IRQMap -> (ObjID, KernelObject Word) -> Doc
printObjID objSizeMap objAddrs irqNode (id, _) =
    constdefs name "cdl_object_id" $+$
    doubleQuotes (printID id <+> equiv <+> obj_id)
    $+$ text ""
    where name = showID id ++ "_id"
          obj_id = getID objSizeMap objAddrs irqNode id

printObjIDs :: ObjectSizeMap -> Map.Map ObjID Word -> ObjMap Word -> IRQMap -> Doc
printObjIDs objSizeMap objAddrs ms irqs =
  vcat (map (printObjID objSizeMap objAddrs irqs) (Map.toList ms))
  $+$ text ""

printObj' :: ObjMap Word -> ObjID -> KernelObject Word -> Doc
printObj' _ _ Endpoint = text "Endpoint"
printObj' _ _ Notification = text "Notification"
printObj' _ id (TCB _ fault _ dom _) = text "Tcb" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_tcb_caps = " ++ capsName id,
    "cdl_tcb_fault_endpoint = " ++ maybe "0" show fault,
    "cdl_tcb_intent = undefined",
    "cdl_tcb_has_fault = " ++ hasFaultEndpoint fault,
    "cdl_tcb_domain = " ++ show dom])
printObj' _ id (CNode _ bits) = text "CNode" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_cnode_caps = " ++ capsName id,
    "cdl_cnode_size_bits = " ++ show bits])
printObj' _ id (ASIDPool _ _) = text "AsidPool" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_asid_pool_caps = " ++ capsName id])
printObj' _ id (PT _) = text "PageTable" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_page_table_caps = " ++ capsName id])
printObj' _ id (PD _) = text "PageDirectory" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_page_directory_caps = " ++ capsName id])
printObj' _ _ (Frame vmSzBits _ _) = text "Frame" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_frame_size_bits = " ++ show vmSzBits])
printObj' _ _ obj = text $ "undefined " ++ isaComment ("unsupported obj: " ++ show obj)

printLemmaObjectSlots :: ObjID -> Doc
printLemmaObjectSlots id =
  lemma (objName++"_object_slots")
        ("object_slots "++objName++" = "++slots)
        ["by (simp add: "++objName++"_def object_slots_def)"]
  where objName = showID id
        slots = capsName id

printObj :: ObjMap Word -> IRQMap -> CoverMap -> (ObjID, KernelObject Word) -> Doc
printObj ms irqNode covers (id, obj) = printCaps ms id irqNode covers obj $+$
    constdefs name "cdl_object" $+$
    doubleQuotes (text name <+> equiv <+> text "Types_D." <> printObj' ms id obj)
    $+$ text "" $+$
    (if hasSlots obj then printLemmaObjectSlots id $+$ text "" else text "")
    where name = showID id

printObjs :: ObjMap Word -> IRQMap -> CoverMap -> [(ObjID, KernelObject Word)] -> Doc
printObjs ms irqNode covers isaCdlObjects =
  vcat (map (printObj ms irqNode covers) isaCdlObjects)

printObjMapping :: (ObjID, KernelObject Word) -> Doc
printObjMapping (id, _) = printID id <+> text ("\\<mapsto> " ++ showID id)

printObjMap :: ObjMap Word -> [(ObjID, KernelObject Word)] -> Doc
printObjMap _ isaCdlObjects =
    case map printObjMapping isaCdlObjects of
        [] -> text "Map.empty"
        xs -> brackets $ fsep $ punctuate comma xs

-- Map of all objects. We use the FastMap builder for scalability.
printObjects :: [(ObjID, KernelObject Word)] -> Doc
printObjects isaCdlObjects =
    text "local_setup {*" $+$
    text "FastMap.define_map (FastMap.name_opts_default \"objects\")" $+$
    nest 2 (
         nest 2 (brackets (vcat $ punctuate comma $ map binding isaCdlObjects)) $+$
         text "@{term \"id :: cdl_object_id \\<Rightarrow> cdl_object_id\"}" $+$
         text "@{thms ids}" $+$
         text "false"
         ) $+$
    text "*}"
    where binding (id, _) = text $ "(@{term \"" ++ showID id ++ "_id\"}, " ++
                                   "@{term \"" ++ showID id ++ "\"})"

printIrqMapping :: (Word, Doc) -> Doc
printIrqMapping (irqID, id) =
    text (hex irqID) <+> text ":=" <+> id

{- The capDL formal model doesn't use a partial mapping for
 - irqs, which is nonsensical: no platform has all possible
 - "cdl_irq" values. This is a bug to be fixed.
 - For now, we just use "undefined" as the default mapping, and
 - add IRQs from our input model on top of that.
 -}

-- Map each known IRQ to its IRQ node address.
printIRQsMap :: ObjMap Word -> IRQMap -> Doc
printIRQsMap _ irqNode =
    let irqs = Map.map printID irqNode
        irqMap = parens $ fsep $ punctuate comma $ map printIrqMapping $
                 Map.toList irqs
        baseIrqs = text "undefined"
        allIrqs | Map.null irqs = baseIrqs -- irqMap would be "()"
                | otherwise = baseIrqs <+> irqMap
    in allIrqs

printIRQs :: ObjMap Word -> IRQMap -> Doc
printIRQs ms irqNode =
    constdefs "irqs" "cdl_irq \\<Rightarrow> cdl_object_id" $+$
    doubleQuotes (text "irqs" <+> equiv <+> printIRQsMap ms irqNode)

makeASIDTable :: ObjMap Word -> CapMap Word
makeASIDTable ms = foldl' addToASIDTable Map.empty (Map.toList ms)
  where addToASIDTable asidTable (objID, ASIDPool _ (Just asidHigh)) =
                Map.insert asidHigh (ASIDPoolCap objID) asidTable
        addToASIDTable asidTable _ = asidTable


printASIDTable :: ObjMap Word -> IRQMap -> CoverMap -> Doc
printASIDTable ms irqs covers =
    constdefs "asid_table" "cdl_cap_map" $+$
    doubleQuotes (text "asid_table" <+> equiv <+>
                            printCapMap ms irqs covers False (makeASIDTable ms))

printCapRef :: CapRef -> Doc
printCapRef (obj, slot) = parens $ printID obj <> comma <+> num slot

printCDTMapping :: (CapRef, CapRef) -> Doc
printCDTMapping (parent, child) =
    printCapRef parent <+> text "\\<mapsto>" <+> printCapRef child

printCDTMap :: CDT -> Doc
printCDTMap cdt =
    case map printCDTMapping (Map.toList cdt) of
        [] -> text "Map.empty"
        xs -> brackets $ fsep $ punctuate comma xs

printCDT :: CDT -> Doc
printCDT cdt = constdefs "cdt" "cdl_cdt" $+$
    doubleQuotes (text "cdt" <+> equiv <+> printCDTMap cdt)

printCDLState :: Arch -> Doc
printCDLState arch =
    record $ fsep $ punctuate comma $ map text ["cdl_arch = " ++ show arch,
        "cdl_objects = objects", "cdl_cdt = cdt",
        "cdl_current_thread = undefined", "cdl_irq_node = irqs",
        "cdl_asid_table = asid_table", "cdl_current_domain = undefined"]

printSimps :: ObjMap Word -> [(ObjID, KernelObject Word)] -> Doc
printSimps ms isaCdlObjects =
    text "lemmas ids = "      $+$ vcat (map objIDDef $ Map.toList ms) $+$ text "" $+$
    text "lemmas cap_defs = " $+$ vcat (map capsDef objsWithCaps) $+$ text "" $+$
    text "lemmas obj_defs = " $+$ vcat (map objDef isaCdlObjects) $+$ text ""
    where objIDDef (id, _) = printID id <> text "_def"
          capsDef (id, _) = text (capsName id) <> text "_def"
          objDef (id, _) = text (showID id) <> text "_def"
          objsWithCaps = filter (\(_, obj) -> hasSlots obj) isaCdlObjects

printState :: Arch -> Doc
printState arch = constdefs "state" "cdl_state" $+$
    doubleQuotes (text "state" <+> equiv <+> printCDLState arch)

printFileName :: String -> Doc
printFileName file = text $ dropExtension $ takeFileName file

printHeader :: String -> Doc
printHeader name =
    text "theory" <+> doubleQuotes (printFileName name) $+$ text "imports" $+$
    nest 2 (text "\"DSpec.Types_D\"" $+$
            text "\"Lib.FastMap\"" $+$
            text ("\"DPolicy.Dpolicy\" " ++ isaComment "@{const ptr_range}")) $+$
    text "begin"

printFooter :: Doc
printFooter = text "end"

printIsabelle :: String -> ObjectSizeMap -> Model Word -> Doc
printIsabelle name objSizeMap (Model (arch@ARM11) ms irqNode cdt untypedCovers) =
    printHeader name $+$ text "" $+$

    printObjIDs objSizeMap objAddrs ms irqNode $+$
    printObjs ms irqNode untypedCovers isaCdlObjects $+$
    text "" $+$
    printSimps ms isaCdlObjects $+$ text "" $+$

    printObjects isaCdlObjects $+$ text "" $+$
    printIRQs ms irqNode $+$ text "" $+$
    printASIDTable ms irqNode untypedCovers $+$ text "" $+$
    printCDT cdt $+$ text "" $+$
    printState arch $+$ text "" $+$

    printFooter
    where objAddrs = case getPhysAddrs objSizeMap ms untypedCovers of
                         Left (msg, ids) ->
                             error $ "Failed to allocate objects from untypeds: " ++
                                     msg ++ " (" ++ intercalate ", " (map show ids) ++ ")"
                         Right x -> x
          getAddr' = getAddr objSizeMap objAddrs irqNode
          -- Isabelle capDL objects. These are sorted by id because the
          -- FastMap builder expects a sorted list as input.
          isaCdlObjects = sortBy (comparing $ getAddr' . fst) $
                          filter (notUntyped . snd) $
                          Map.toList ms

printIsabelle _ _ _ =
    error "Currently only the ARM11 architecture is supported when parsing to Isabelle"
