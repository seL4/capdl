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

module CapDL.PrintModel where

import CapDL.Model
import CapDL.PrintUtils

import Text.PrettyPrint
import Data.List.Compat
import Prelude ()
import Prelude.Compat
import qualified Data.Map as Map
import qualified Data.Set as Set

indent = 2

prettyMapping :: (Show a, Show b) => (a,b) -> Doc
prettyMapping (a,b) = text (show a) <> text " -> " <> text (show b)

prettyBigList f xs = lbrack $+$ (nest indent (vcat (map f xs))) $+$ rbrack

prettyMap m = prettyBigList prettyMapping (Map.toList m)

prettyNum :: Int -> Doc
prettyNum num = brackets $ int num

prettyNameDecl :: ObjID -> Int-> Doc
prettyNameDecl (n, Nothing) _ = text n <+> equals
prettyNameDecl (n, _) num = text n <> prettyNum num <+> equals

prettyNameRefr :: [ObjID]-> Doc
prettyNameRefr objs = (text.fst.head) objs <> (prettyBrackets $ map snd objs)

prettyObject :: ObjID -> Int -> KernelObject a -> Doc
prettyObject _ _ Untyped {} = empty
prettyObject n num obj = prettyNameDecl n num <+> prettyObjParams obj

prettyObjectsList :: [(ObjID, KernelObject a)] -> [Doc]
prettyObjectsList [] = []
prettyObjectsList list@((id, obj):_) =
    prettyObject id len obj:prettyObjectsList (drop len list)
    where len = length (takeWhile (sameName id) (map fst list))

prettyObjects :: ObjMap a -> Doc
prettyObjects m = vcat (prettyObjectsList (Map.toList m))

prettyCovered :: [ObjID] -> [Doc]
prettyCovered [] = []
prettyCovered list@(id:_) =
    prettyNameRefr same:prettyCovered (drop (length same) list)
    where same = takeWhile (sameName id) list

getCover :: ObjID -> CoverMap -> ObjSet
getCover ut covers =
    case Map.lookup ut covers of
        Nothing -> Set.empty
        Just cover -> cover

prettyIndexedUntyped :: CoverMap -> [(ObjID, KernelObject a)] -> Doc
prettyIndexedUntyped _ [] = empty
prettyIndexedUntyped covers ((name, obj@(Untyped _ _)):xs) =
    if Set.null cover
    then prettyIndexedUntyped covers xs
    else prettyNameRefr [name] <+> equals <+> prettyObjParams obj <+>
        braces (fsep $ punctuate comma $ prettyCovered $ Set.toList cover) $+$
        prettyIndexedUntyped covers xs
    where cover = getCover name covers
prettyIndexedUntyped _ _ = error "Untyped only"

prettyUntyped :: CoverMap -> [(ObjID, KernelObject a)] -> Doc
prettyUntyped covers list@((name, obj@(Untyped _ _)):_) =
    if snd name == Nothing
    then prettyNameDecl name len <+> prettyObjParams obj <+>
        if Set.null cover
        then empty
        else braces (fsep $ punctuate comma $ prettyCovered $ Set.toList cover)
    else prettyNameDecl name len <+> prettyObjParams obj $+$
         prettyIndexedUntyped covers list
    where len = length list
          cover = getCover name covers
prettyUntyped _ _ = empty

prettyUntypedsList :: CoverMap -> [(ObjID, KernelObject a)] -> [Doc]
prettyUntypedsList _ [] = [empty]
prettyUntypedsList covers list@((name, _):_) =
    prettyUntyped covers (take len list) :
    prettyUntypedsList covers (drop len list)
    where len = length $ takeWhile (sameName name) (map fst list)

prettyUntypeds :: ObjMap a -> CoverMap -> Doc
prettyUntypeds m covers = vcat (prettyUntypedsList covers (Map.toList m))

prettyCap :: Cap -> [Maybe Word] -> Doc
prettyCap cap range = printCap cap <> prettyBrackets range <+> maybeCapParams cap

prettySlot :: Printing a => (a, Cap) -> [Maybe Word] -> Doc
prettySlot (n, cap) range = num n <> colon <+> prettyCap cap range

prettySlotsRange :: Printing a => [(a, Cap)] -> Doc
prettySlotsRange [] = error "empty"
prettySlotsRange list@(x:_) =
    prettySlot x (map (snd.objID.snd) list)

prettySlotsList :: Printing a => [(a, Cap)] -> [Doc]
prettySlotsList [] = []
prettySlotsList list@(first@(_, cap):xs) =
    if hasObjID cap
    then prettySlotsRange sameGroup : prettySlotsList (drop (length sameGroup) list)
    else prettySlot first [Nothing] : prettySlotsList xs
    where sameGroup = sameArray list

capHead :: ObjID -> [Maybe Word] -> Doc
capHead (name, _) range = text name <> prettyBrackets range

prettySlots :: Printing a => (ObjID, KernelObject a) -> [Maybe Word] -> Doc
prettySlots (n, obj) range =
    let xs = prettySlotsList $ Map.toList $ slots obj
    in case xs of
        [] -> empty
        [slot] -> capHead n range <+> braces slot $+$ text ""
        xs -> hang (capHead n range <+> lbrace) indent (vcat xs)
              $+$ rbrace $+$ text ""

prettyCapsGroup :: Printing a => [(ObjID, KernelObject a)] -> Doc
prettyCapsGroup list = prettySlots (head list) (map (snd.fst) list)

prettyCapsList :: Printing a => [(ObjID, KernelObject a)] -> [Doc]
prettyCapsList [] = []
prettyCapsList list@(first:xs)
    | hasSlots (snd first) =
        prettyCapsGroup sameCaps : prettyCapsList otherCaps
    | otherwise = prettyCapsList xs
    where (sameCaps, otherCaps) = partition (same first) list

prettyCaps :: Printing a => ObjMap a -> Doc
prettyCaps ms = vcat $ prettyCapsList $ Map.toList ms

prettyCapRef :: CapRef -> Doc
prettyCapRef (obj, slot) = parens $ text (showID obj) <> comma <+> num slot

prettyCDTDecl :: CapRef -> [CapRef] -> Doc
prettyCDTDecl parent children =
    let parent' = prettyCapRef parent
        children' = map prettyCapRef children
    in case children' of
        [child] -> parent' <+> braces child $+$ text ""
        children -> hang (parent' <+> lbrace) indent (vcat children)
            $+$ rbrace $+$ text ""

prettyCDTGroup :: [(CapRef, CapRef)] -> Doc
prettyCDTGroup list = prettyCDTDecl (snd (head list)) (map fst list)

prettyCDTList :: [(CapRef, CapRef)] -> [Doc]
prettyCDTList [] = []
prettyCDTList list =
    prettyCDTGroup sameParents : prettyCDTList otherParents
    where
        (sameParents, otherParents) =
            partition ((== firstParent) . snd) list
        firstParent = snd $ head list

prettyCDT :: CDT -> Doc
prettyCDT cdt = vcat $ prettyCDTList $ Map.toList cdt

prettyIRQ :: ObjID -> [Maybe Word] -> Doc
prettyIRQ irq range = text (fst irq) <> prettyBrackets range

prettyIRQSlot :: (Word, ObjID) -> [Maybe Word] -> Doc
prettyIRQSlot (n, irq) range = num n <> colon <+> prettyIRQ irq range

prettyIRQSlotsRange :: [(Word, ObjID)] -> Doc
prettyIRQSlotsRange [] = error "empty"
prettyIRQSlotsRange list@(x:_) =
    prettyIRQSlot x (map (snd.snd) list)

prettyIRQSlotsList :: [(Word, ObjID)] -> [Doc]
prettyIRQSlotsList [] = []
prettyIRQSlotsList list =
    prettyIRQSlotsRange sameGroup : prettyIRQSlotsList (drop (length sameGroup) list)
    where sameGroup = sameArray list

prettyIRQNode :: IRQMap -> Doc
prettyIRQNode irqNode =
    let irqs = prettyIRQSlotsList (Map.toList irqNode)
    in case irqs of
        [] -> empty
        [irq] -> irq $+$ text ""
        irqs -> vcat irqs $+$ text ""

prettyMappings :: Printing a => Model a -> Doc
prettyMappings (Model _ ms irqNode cdt untypedCovers) =
    text "objects {" $+$
    text "" $+$
    nest indent (prettyObjects ms) $+$
    text "" $+$
    nest indent (prettyUntypeds ms untypedCovers) $+$
    text "" $+$
    text "} caps {" $+$
    text "" $+$
    nest indent (prettyCaps ms) $+$
    text "} cdt {" $+$
    text "" $+$
    nest indent (prettyCDT cdt) $+$
    text "} irq maps {" $+$
    text "" $+$
    nest indent (prettyIRQNode irqNode) $+$
    text "}"

prettyHeader arch =
    text "arch" <+> prettyArch arch $+$
    text ""

pretty model =
    prettyHeader (arch model) $+$
    prettyMappings model
