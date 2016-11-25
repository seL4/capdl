--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

module CapDL.PrintIsabelle where

import CapDL.Model
import CapDL.PrintUtils (logBase2, printAsid)

import Text.PrettyPrint
import Data.List.Compat
import Prelude ()
import Prelude.Compat
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Bits
import System.FilePath.Posix

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

equiv = text "\\<equiv>"

lambda = text "\\<lambda>"

showID :: ObjID -> String
showID (name, Nothing) = name
showID (name, Just num) = name ++ "_" ++ show num

capsName id = showID id ++ "_caps"

constdefs :: String -> String -> Doc
constdefs name typ =
    text ("definition " ++ name ++ " :: ") <> doubleQuotes (text typ) <+>
    text "where"

lemma :: String -> String -> String -> Doc
lemma name statement proof =
  text ("lemma " ++ name ++ ": ") <> doubleQuotes (text statement) $$
  nest 2 (text proof)

lemma' :: String -> [String] -> String -> Doc
lemma' name statements proof =
  text ("lemma " ++ name ++ ": ") $+$ vcat (map (doubleQuotes . text) statements) $$
  nest 2 (text proof)

record :: Doc -> Doc
record p = text "\\<lparr>" <+> p <+> text "\\<rparr>"

printSet :: [Doc] -> Doc
printSet ls = braces $ hsep $ punctuate comma ls

filterEmpty :: [Doc] -> [Doc]
filterEmpty = filter (not . isEmpty)

notUntyped Untyped {} = False
notUntyped _ = True

getID :: ObjMap Word -> IRQMap -> ObjID -> Doc
getID ms irqNode id =
    case lookupElem id irqNode of
        Just irq -> int (Map.size ms' + fromIntegral irq)
        Nothing ->
            case Map.lookup id ms' of
                Just _ -> int (Map.findIndex id ms')
                Nothing -> empty
    where ms' = Map.filterWithKey (\id _ -> not (mapElem id irqNode)) ms

printID :: ObjID -> Doc
printID id = text (showID id ++ "_id")

printRight :: Rights -> Doc
printRight Read = text "Read"
printRight Write = text "Write"
printRight Grant = text "Grant"

printRightsList :: [Rights] -> [Doc]
printRightsList [] = [empty]
printRightsList xs = map printRight xs

printRights :: CapRights -> Doc
printRights r =
    printSet $ printRightsList $ Set.toList r

getObject :: ObjID -> ObjMap Word -> Maybe (KernelObject Word)
getObject = Map.lookup

bitsToPageBits :: Word -> Int
bitsToPageBits sz = logBase2 sz 0

printSize :: ObjID -> ObjMap Word -> Doc
printSize id ms =
    let Just (Frame sz _) = getObject id ms
    in num $ bitsToPageBits sz

printCoverSet :: ObjMap Word -> Maybe ObjSet -> Doc
printCoverSet _ Nothing = printSet []
printCoverSet ms (Just objs) = printSet $ map printID $ Set.toList objs'
    where objs' = Set.filter (\id -> isJust $ Map.lookup id ms) objs

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

-- The bool represents whether the cap is a real cap;
-- i.e. if it is not in a PT or PD
printCap :: ObjMap Word -> IRQMap -> CoverMap -> Bool -> Cap -> Doc
printCap _ _ _ _ NullCap = text "NullCap"
printCap ms _ covers _ (UntypedCap id) =
    text "UntypedCap" <+> printCoverSet ms (Map.lookup id covers) <+>
    printSet []
printCap _ _ _ _ (EndpointCap id badge rights) = text "EndpointCap" <+>
    printID id <+> num badge <+> printRights rights
-- TODO: Rename AsyncEndpointCap to NotificationCap
printCap _ _ _ _ (NotificationCap id badge rights) = text "AsyncEndpointCap" <+>
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
    printID id <+> printRights rights <+> printSize id ms <+>
    printReal real <+> printMaybeAsid asid <+>
    text (if cached then "" else "(* uncached *)")
printCap _ _ _ real (PTCap id asid) =
    text "PageTableCap" <+> printID id <+> printReal real <+>
    printMaybeAsid asid
printCap _ _ _ real  (PDCap id asid) =
    text "PageDirectoryCap" <+> printID id <+> printReal real <+>
    printMaybeAsid asid
printCap _ _ _ _ ASIDControlCap = text "AsidControlCap"
printCap _ _ _ _ (ASIDPoolCap id asid) =
    text "AsidPoolCap" <+> printID id <+> printAsid asid
printCap _ _ _ _ _ = error "IO caps unsupported"

printCapMapping :: ObjMap Word -> IRQMap -> CoverMap -> Bool -> (Word, Cap) -> Doc
printCapMapping ms irqNode covers real (slot, cap) =
    num slot <+> text "\\<mapsto>"
                      <+> text "Types_D." <> printCap ms irqNode covers real cap

printCapMap :: ObjMap Word -> IRQMap -> CoverMap -> Bool -> CapMap Word -> Doc
printCapMap ms irqNode covers real slots =
    case map (printCapMapping ms irqNode covers real) (Map.toList slots) of
        [] -> text "empty"
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
printCaps ms id irqNode covers (ASIDPool slots) =
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

printObjID :: ObjMap Word -> IRQMap -> (ObjID, KernelObject Word) -> Doc
printObjID ms irqNode (id, _) =
    constdefs name "cdl_object_id" $+$
    doubleQuotes (printID id <+> equiv <+> obj_id)
    $+$ text ""
    where name = showID id ++ "_id"
          obj_id = getID ms irqNode id

printObjIDs :: ObjMap Word -> IRQMap -> Doc
printObjIDs ms irqs = vcat (map (printObjID ms irqs) (Map.toList ms)) $+$ text ""

printObj' :: ObjMap Word -> ObjID -> KernelObject Word -> Doc
printObj' _ _ Endpoint = text "Endpoint"
printObj' _ _ Notification = text "AsyncEndpoint" -- TODO: Rename AsyncEndpoint to Notification
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
printObj' _ id (ASIDPool _) = text "AsidPool" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_asid_pool_caps = " ++ capsName id])
printObj' _ id (PT _) = text "PageTable" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_page_table_caps = " ++ capsName id])
printObj' _ id (PD _) = text "PageDirectory" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_page_directory_caps = " ++ capsName id])
printObj' _ _ (Frame vmSz _) = text "Frame" <+>
    record (fsep $ punctuate comma $ map text
    ["cdl_frame_size_bits = " ++ show (bitsToPageBits vmSz)])
printObj' _ _ _ = error "Untyped and IO objs unsupported"

printLemmaObjectSlots :: ObjID -> Doc
printLemmaObjectSlots id = 
  lemma (objName++"_object_slots") 
        ("object_slots "++objName++" = "++slots)
        ("by (simp add: "++objName++"_def object_slots_def)")
  where objName = showID id
        slots = capsName id
  


printObj :: ObjMap Word -> IRQMap -> CoverMap -> (ObjID, KernelObject Word) -> Doc
printObj ms irqNode covers (id, obj) = printCaps ms id irqNode covers obj $+$
    constdefs name "cdl_object" $+$
    doubleQuotes (text name <+> equiv <+> text "Types_D." <> printObj' ms id obj)
    $+$ text "" $+$
    (if hasSlots obj then printLemmaObjectSlots id $+$ text "" else text "")
    where name = showID id

-- This will cause a problem if an actual object is called empty_irq_node
printEmptyIrqNode :: Doc
printEmptyIrqNode = constdefs "empty_irq_node" "cdl_object" $+$
    doubleQuotes (text "empty_irq_node"  <+> equiv <+> text "Types_D.CNode" <+>
    record (fsep $ punctuate comma $
                map text ["cdl_cnode_caps = empty", "cdl_cnode_size_bits = 0"]))

printObjs :: ObjMap Word -> IRQMap -> CoverMap -> Doc
printObjs ms irqNode covers = vcat (map (printObj ms irqNode covers) (Map.toList ms)) $+$
    printEmptyIrqNode $+$ text ""

printObjMapping :: (ObjID, KernelObject Word) -> Doc
printObjMapping (id, _) = printID id <+> text ("\\<mapsto> " ++ showID id)

printEmptyIrqObjMapping :: ObjMap Word -> IRQMap -> Doc
printEmptyIrqObjMapping ms irqNode =
    parens (lambda <> text ("obj_id. if " ++ start ++ " \\<le> obj_id \\<and> obj_id \\<le> " ++ end) <+>
        text "then (Some empty_irq_node)" <+> text "else None")
    where ms' = Map.filterWithKey (\id _ -> not (mapElem id irqNode)) ms
          start = show $ Map.size ms
          end = show $ Map.size ms' + 255

printEmptyIrqObjMap :: ObjMap Word -> IRQMap -> Doc
printEmptyIrqObjMap ms irqNode =
    constdefs "empty_irq_objects" "cdl_object_id \\<Rightarrow> cdl_object option" $+$
    doubleQuotes (text "empty_irq_objects" <+> equiv <+> printEmptyIrqObjMapping ms irqNode)

printObjMap :: ObjMap Word -> IRQMap -> Doc
printObjMap ms _ = text "empty_irq_objects ++" $+$
    case map printObjMapping (Map.toList ms) of
        [] -> text "empty"
        xs -> brackets $ fsep $ punctuate comma xs

printObjects :: ObjMap Word -> IRQMap -> Doc
printObjects ms irqNode =
    printEmptyIrqObjMap ms irqNode $+$ text "" $+$
    constdefs "objects" "cdl_object_id \\<Rightarrow> cdl_object option" $+$
    doubleQuotes (text "objects" <+> equiv <+> printObjMap ms irqNode)

printIrqMapping :: (Int, Doc) -> Doc
printIrqMapping (irqID, id) =
    int irqID <+> text ":=" <+> id

completeMap :: Ord a => [(a, b)] -> [(a, b)] -> [(a, b)]
completeMap [] [] = []
completeMap [] y = y
completeMap x [] = x
completeMap x@((x1, x2):xs) y@((y1, y2):ys)
    | x1 < y1 = (x1, x2) : completeMap xs y
    | x1 == y1 = (x1, x2) : completeMap xs ys
    | otherwise = (y1, y2) : completeMap x ys

printIrqMappings :: ObjMap Word -> IRQMap -> [Doc]
printIrqMappings ms irqNode =
    let irqs = Map.map printID irqNode
        irqs' = Map.toList $ Map.mapKeys fromIntegral irqs
        ids = completeMap irqs' (zip [0..255] (map num [Map.size ms'..])) --FIXME: factor out 255?
    in map printIrqMapping ids
    where ms' = Map.filterWithKey (\id _ -> not (mapElem id irqNode)) ms

printIRQsMap :: ObjMap Word -> IRQMap -> Doc
printIRQsMap ms irqNode =
    parens $ fsep $ punctuate comma (printIrqMappings ms irqNode)

printIRQs :: ObjMap Word -> IRQMap -> Doc
printIRQs ms irqNode =
    constdefs "irqs" "cdl_irq \\<Rightarrow> cdl_object_id" $+$
    doubleQuotes (text "irqs" <+> equiv <+> text "undefined"
                                                   <+> printIRQsMap ms irqNode)

addToASIDTable' :: CapMap Word -> Cap -> CapMap Word
addToASIDTable' asidTable cap = case cap of
    ASIDPoolCap obj asid ->
        case Map.lookup (fst asid) asidTable of
            Nothing -> Map.insert (fst asid) (ASIDPoolCap obj (0, 0)) asidTable
            Just (ASIDPoolCap obj' _) ->
                if obj == obj'
                then asidTable
                else error $ "Multiple ASIDPools mapped to " ++ show asid
            _ -> error "inconceivable: not an ASIDPoolCap"
    _ -> asidTable

addToASIDTable :: CapMap Word -> KernelObject Word -> CapMap Word
addToASIDTable asidTable obj
    | hasSlots obj = foldl' addToASIDTable' asidTable (map snd $ Map.toList $ slots obj)
    | otherwise = asidTable

makeASIDTable :: ObjMap Word -> CapMap Word
makeASIDTable ms = foldl' addToASIDTable Map.empty (map snd $ Map.toList ms)

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
        [] -> text "empty"
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

printSimps :: ObjMap Word -> Doc
printSimps ms =
    text "lemmas ids = "      $+$ vcat (map obj_ids obj_list) $+$ text "" $+$
    text "lemmas cap_defs = " $+$ vcat (map caps objs_with_caps) $+$ text "" $+$
    text "lemmas obj_defs = " $+$ vcat (map objs obj_list) $+$ text "" $+$
    lemma' "objects" (map objects obj_list) ("by (auto simp: objects_def ids)")
    where obj_ids (id, _) = printID id <> text "_def"
          caps (id, _) = text (capsName id) <> text "_def"
          objs (id, _) = text (showID id) <> text "_def"
          obj_list = Map.toList ms
          objs_with_caps = filter (\(_, obj) -> hasSlots obj) obj_list
          objects (id, _) = "objects " ++ showID id ++ "_id = Some " ++ showID id

printState :: Arch -> Doc
printState arch = constdefs "state" "cdl_state" $+$
    doubleQuotes (text "state" <+> equiv <+> printCDLState arch)

printFileName :: String -> Doc
printFileName file = text $ dropExtension $ takeFileName file

printHeader :: String -> Doc
printHeader name =
    text "theory" <+> doubleQuotes (printFileName name) $+$ text "imports \"~~/../l4v/spec/capDL/Types_D\""
    $+$ text "begin"

printFooter :: Doc
printFooter = text "end"

printIsabelle :: String -> Model Word -> Doc
printIsabelle _ (Model IA32 _ _ _ _) =
    error "Currently only the ARM11 architecture is supported when parsing to Isabelle"
printIsabelle name (Model arch ms irqNode cdt untypedCovers) =
    printHeader name $+$ text "" $+$
    printObjIDs ms' irqNode $+$
    printObjs ms' irqNode untypedCovers $+$
    printObjects ms' irqNode $+$ text "" $+$
    printIRQs ms' irqNode $+$ text "" $+$
    printASIDTable ms' irqNode untypedCovers $+$ text "" $+$
    printCDT cdt $+$ text "" $+$
    printState arch $+$ text "" $+$
    printSimps ms' $+$ text "" $+$
    printFooter
    where ms' = Map.filter notUntyped ms
