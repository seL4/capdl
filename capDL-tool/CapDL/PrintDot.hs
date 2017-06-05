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
module CapDL.PrintDot where

import CapDL.Model
import CapDL.PrintUtils

import Text.PrettyPrint
import Data.List.Compat
import Prelude ()
import Prelude.Compat
import qualified Data.Map as Map
import qualified Data.Set as Set

indent = 2

type NodeMap = Map.Map ObjID String

class (Printing a) => DotPrinting a where
    dotRange :: a -> Int -> Doc

instance DotPrinting Word where
    dotRange n 1 = num n
    dotRange n len = num n <> text " - " <> num (n + fromIntegral len - 1)

instance DotPrinting [Word] where
    dotRange ns 1 =  num ns
    dotRange [n] len = dotRange n len
    dotRange ns len =
        let n = last ns
        in num (init ns) <> comma <+> dotRange n len

angles :: String -> Doc
angles st = text ("<" ++ st ++ ">")

getName :: ObjID -> NodeMap -> String
getName id names =
    case Map.lookup id names of
        Just name -> name
        Nothing -> error $ "Something weird happened when printing: " ++ showID id

getObject :: ObjMap a -> ObjID -> KernelObject a
getObject ms id =
    let Just obj = Map.lookup id ms
    in obj

sameNode :: NodeMap -> ObjID -> ObjID -> Bool
sameNode names first second = getName first names == getName second names

dotObjParams :: KernelObject a -> Doc
dotObjParams obj = text " \\n " <> prettyObjParams obj

hasCover :: ObjID -> CoverMap -> Bool
hasCover ut covers =
    case Map.lookup ut covers of
        Nothing -> False
        Just cover -> not $ Set.null cover

dotCovered :: NodeMap -> ObjMap a -> CoverMap -> [ObjID] -> [Doc]
dotCovered _ _ _ [] = []
dotCovered names ms covers list@(id:_)
    | hasCover id covers =
        dotUntyped names ms covers [] id [Nothing] obj:
            dotCovered names ms covers (drop (length same) list)
    | otherwise =
        (doubleQuotes.text) (getName id names) <> semi:
                dotCovered names ms covers (drop (length same) list)
        where
            same = takeWhile (sameNode names id) list
            obj = getObject ms id

-- Minor problem if a covered object has a cap to the untyped
dotCluster ::  NodeMap -> ObjMap a -> CoverMap -> ObjID -> [Maybe Word] ->
              [ObjID] -> Doc
dotCluster names ms covers n _ cover =
    text "subgraph" <+> (doubleQuotes.text) ("cluster_" ++ getName n names) <+>
    braces (braces (text "rank = source;" <+> (doubleQuotes.text) (getName n names)
            <+> text "[style = filled];") <> semi <+>
            hsep (dotCovered names ms covers cover)) <> semi

getCover :: ObjID -> CoverMap -> ObjSet
getCover ut covers =
    case Map.lookup ut covers of
        Nothing -> Set.empty
        Just cover -> cover

dotUntyped :: NodeMap -> ObjMap a -> CoverMap -> [ObjID] -> ObjID ->
              [Maybe Word] -> KernelObject a -> Doc
dotUntyped names ms covers cov id@(n, _) range obj@(Untyped {}) =
    if id `notElem` cov
    then if Set.null cover
    then (doubleQuotes.text) (getName id names) <+> brackets (text "label ="
        <+> doubleQuotes (braces (angles "name" <+> text n
        <> prettyBrackets range <> dotObjParams obj))) <> semi
    else dotCluster names ms covers id range (Set.toList cover)
    else empty
    where cover = getCover id covers
dotUntyped _ _ _ _ _ _ _ = empty

--Do we want the port to be s
dotEdge :: DotPrinting a => NodeMap -> a -> Cap -> ObjID -> Doc
dotEdge names slot cap n =
    (doubleQuotes.text) (getName n names) <> text (":\"t" ++ show slot ++ "\"")
    <> text ":s ->" <+> (doubleQuotes.text) (getName (objID cap) names)
    <> text ":Object" <> semi

sameHeadNode :: NodeMap -> (a, Cap) -> (a, Cap) -> Bool
sameHeadNode names (_, first) (_, second) =
    sameNode names (objID first) (objID second)

dotEdges :: DotPrinting a => NodeMap -> [(a, Cap)] -> a -> ObjID -> Doc
dotEdges _ [] _ _ = empty
dotEdges names list@(first@(_, cap):_) slot n =
    dotEdge names slot cap n $+$ dotEdges names others slot n
    where (_,others) = partition (sameHeadNode names first) list

dotEdgesList :: DotPrinting a => NodeMap -> [(a, Cap)] -> ObjID -> [Doc]
dotEdgesList _ [] _ = []
dotEdgesList names list@((slot, cap):xs) name =
    if hasObjID cap
    then dotEdges names sameGroup slot name:
        dotEdgesList names (drop (length sameGroup) list) name
    else dotEdgesList names xs name
    where sameGroup = sameArray list

dotTop :: DotPrinting a => a -> Int -> Doc
dotTop n len = angles ("a" ++ show n) <+> dotRange n len

dotBot :: DotPrinting a => a -> Cap -> [Maybe Word] -> Doc
dotBot n cap range = angles ("t" ++ show n) <+>
    prettyBrackets range <+> maybeCapParams cap

dotSlot :: DotPrinting a => (a, Cap) -> [Maybe Word] -> Doc
dotSlot (n, cap) range = text "|" <>
    braces (dotTop n (length range) <+> text "|" <+> dotBot n cap range)

dotSlotsRange :: DotPrinting a => [(a, Cap)] -> Doc
dotSlotsRange [] = error "dotSlotsRange: empty"
dotSlotsRange list@(x:_) =
    dotSlot x (map (snd.objID.snd) list)

dotSlotsList :: DotPrinting a => [(a, Cap)] -> [Doc]
dotSlotsList [] = []
dotSlotsList list@(first@(_, cap):xs) =
    if hasObjID cap
    then dotSlotsRange sameGroup:dotSlotsList (drop (length sameGroup) list)
    else (dotSlot first [Nothing]):(dotSlotsList xs)
    where sameGroup = sameArray list

dotNodeHead :: ObjID -> KernelObject a -> [Maybe Word] -> Doc
dotNodeHead (name, _) obj range =
    text "label" <+> equals <+> text "\"" <>
    braces (angles "Object" <+> text name <> prettyBrackets range
            <> dotObjParams obj)

dotNode :: DotPrinting a => NodeMap -> ObjMap a -> CoverMap -> [ObjID]
           -> (ObjID, KernelObject a) -> [Maybe Word] -> Doc
dotNode names ms covers cov (n, obj) range =
    let xs = (if hasSlots obj then Map.toList $ slots obj else [])
    in nest indent ((doubleQuotes.text) (getName n names) <+>
        brackets (dotNodeHead n obj range <> hcat (dotSlotsList xs)
        <> text "\"") <> semi $+$ nest indent (vcat (dotEdgesList names xs n)
        $+$ if hasCover n covers
            then dotUntyped names ms covers cov n range obj
            else empty))

dotNodesGroup :: DotPrinting a => NodeMap -> ObjMap a -> CoverMap -> [ObjID]
                 -> [(ObjID, KernelObject a)] -> Doc
dotNodesGroup names ms covers cov list =
    dotNode names ms covers cov (head list) (map (snd.fst) list)

dotNodesList :: DotPrinting a => NodeMap -> ObjMap a -> CoverMap -> [ObjID]
                -> [(ObjID, KernelObject a)] -> [Doc]
dotNodesList _ _ _ _ [] = []
dotNodesList names ms covers cov list@(first:_) =
    dotNodesGroup names ms covers cov sameCaps:
    dotNodesList names ms covers cov otherCaps
    where (sameCaps, otherCaps) =
            partition (\tuple -> sameNode names (fst first) (fst tuple)) list

dotNodes :: DotPrinting a => NodeMap -> ObjMap a -> CoverMap -> [ObjID]
            -> [(ObjID, KernelObject a)] -> Doc
dotNodes names ms covers covered list =
    (vcat (dotNodesList names ms covers covered list)) $+$ text ""

getCovered :: ObjMap a -> CoverMap -> [ObjID] -> [ObjID]
getCovered _ _ [] = []
getCovered ms covers (x:xs)
    | hasCover x covers = x : getCovered ms covers xs
    | otherwise = getCovered ms covers xs

getCovUntyped :: ObjMap a -> CoverMap -> [ObjID]
getCovUntyped ms covers =
    concat $ Map.elems $
                  Map.map (\set -> getCovered ms covers (Set.toList set)) covers

initCovNamesGroup :: NodeMap -> [ObjID] -> NodeMap
initCovNamesGroup names list =
    foldl (\map id -> Map.insert id ("cov_" ++ showID (head list)) map)
          names list

initCovNamesCovered :: NodeMap -> ObjSet -> NodeMap
initCovNamesCovered names cover =
    let list = Set.toList cover
    in foldl initCovNamesGroup names
                                                (groupBy (sameNode names) list)

initCovNames :: NodeMap -> CoverMap -> NodeMap
initCovNames names covers =
    foldl initCovNamesCovered names
                                                  (map snd (Map.toList covers))

initNamesGroup :: NodeMap -> [ObjID] -> NodeMap
initNamesGroup names list =
    foldl (\map id -> Map.insert id (showID (head list)) map) names list

initNamesList :: DotPrinting a => NodeMap -> [(ObjID, KernelObject a)] -> NodeMap
initNamesList _ [] = Map.empty
initNamesList names list@(first:_) =
    Map.union (initNamesGroup names (map fst sameCaps))
                (initNamesList names otherCaps)
    where (sameCaps, otherCaps) = partition (same first) list

initNames :: DotPrinting a => [(ObjID, KernelObject a)] -> NodeMap
initNames = initNamesList Map.empty

dotLabel :: Arch -> Doc
dotLabel arch = text "fontsize = 30; labelloc = top; label = \"arch"
    <+> prettyArch arch <> text "\";"

-- nodesep? ranksep? page? concentrate? minlen? fontsize?
dotAttributes :: Doc
dotAttributes = text "nodesep = 0.5; ranksep = 3;"
    $+$ text "node [shape = record]; edge [minlen = 2];" $+$ text ""

dotHeader :: String -> Doc
dotHeader name = text "digraph" <+> doubleQuotes (text name)
    <+> lbrace $+$ dotAttributes

printDot' :: DotPrinting a => String -> Arch -> ObjMap a -> CoverMap -> Doc
printDot' name arch ms covers =
    let list = (Map.toList ms)
        names = initNames list
        names' = initCovNames names covers
        covered = getCovUntyped ms covers
    in dotHeader name $+$
       dotNodes names' ms covers covered list $+$
       dotLabel arch $+$
       rbrace

printDot :: DotPrinting a => String -> Model a -> Doc
printDot name (Model arch ms _ _ untypedCovers) =
    printDot' name arch ms untypedCovers
