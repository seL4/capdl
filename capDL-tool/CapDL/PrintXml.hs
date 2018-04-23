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
module CapDL.PrintXml (printXml) where

import CapDL.Model
import CapDL.PrintUtils

import Text.PrettyPrint
import Data.Maybe (fromMaybe)
import Prelude ()
import Prelude.Compat
import qualified Data.Map as Map
import qualified Data.Set as Set

indent = 4

-- XML header
xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

--
-- Convert a list of key/value pairs into an XML tag attribute string.
--
-- > showXmlAttrsString [("name", "rover"), ("colour", "red")]
-- 'name="rover" colour="red"'
--
showXmlAttrsString :: [(String, String)] -> String
showXmlAttrsString attrs = foldl (\a b -> a ++ " " ++ b) "" joinedAttrs
  where
    joinedAttrs = (map (\(a, b) -> a ++ "=\"" ++ b ++ "\"") attrs)

--
-- Create an XML opening tag, filled with the given attributes.
--
openTag :: String -> [(String, String)] -> String
openTag x [] = "<" ++ x ++ ">"
openTag x attrs = "<" ++ x ++ attrsString ++ ">"
  where
    attrsString = showXmlAttrsString attrs

--
-- Create an XML closing tag.
--
closeTag :: String -> String
closeTag x = "</" ++ x ++ ">"

--
-- Create an empty XML tag.
--
emptyTag :: String -> [(String, String)] -> String
emptyTag x [] = "<" ++ x ++ " />"
emptyTag x attrs = "<" ++ x ++ attrsString ++ " />"
  where
    attrsString = showXmlAttrsString attrs

--
-- Surround the given document with the given tag.
--
xmlSurround :: String -> [(String, String)] -> Doc -> Doc
xmlSurround tag attrs details =
    (text (openTag tag attrs)) $+$ (nest indent details) $+$ (text (closeTag tag))

showCapRef :: CapRef -> String
showCapRef (obj, slot) = "(" ++ showID obj ++ ", " ++ show slot ++ ")"

--
-- Convert CapRights into a string.
--
showRights :: CapRights -> String
showRights rights = readRight ++ writeRight ++ grantRight
    where
        readRight = if Read `Set.member` rights then "r" else ""
        writeRight = if Write `Set.member` rights then "w" else ""
        grantRight = if Grant `Set.member` rights then "g" else ""

--
-- Get an object's attributes.
--
showObjectAttrs :: KernelObject a -> [(String, String)]
showObjectAttrs (TCB _ _ _ domain _) = [("domain", show domain)]
showObjectAttrs (CNode _ sz) = [("size", show sz)]
showObjectAttrs (Untyped (Just sz) paddr) = [("size", show sz), ("paddr", show $ fromMaybe 0 paddr)]
showObjectAttrs (Frame sz paddr _) = [("size", show (logBase2 sz 0)), ("paddr", show $ fromMaybe 0 paddr)]
showObjectAttrs (IOPorts sz) = [("size", show sz)]
showObjectAttrs (IODevice _ dom pci) = [("domain", show dom), ("device", show pci)]
showObjectAttrs (ARMIODevice _ iospace) = [("iospace", show iospace)]
showObjectAttrs (IOPT _ level) = [("level", show level)]
showObjectAttrs _ = []

--
-- Get an object's name.
--
showObjectName :: KernelObject a -> String
showObjectName Endpoint = "Endpoint"
showObjectName Notification = "AsyncEndpoint" -- TODO: Rename AsyncEndpoint to Notification
showObjectName TCB {} = "TCB"
showObjectName CNode {} = "CNode"
showObjectName Untyped {} = "Untyped"
showObjectName ASIDPool {} = "ASIDPool"
showObjectName PT {} = "PT"
showObjectName PD {} = "PD"
showObjectName PDPT {} = "PDPT"
showObjectName PML4 {} = "PML4"
showObjectName Frame {} = "Frame"
showObjectName IOPorts {} = "IOPorts"
showObjectName IODevice {} = "IODevice"
showObjectName ARMIODevice {} = "ARMIODevice"
showObjectName IOPT {} = "IOPT"
showObjectName VCPU {} = "VCPU"
showObjectName SC {} = "SC"
showObjectName RTReply {} = "RTReply"
showObjectName IOAPICIrq {} = "IOAPICIrq"
showObjectName MSIIrq {} = "MSIIrq"

--
-- Get a cap's name.
--
showCapName :: Cap -> String
showCapName NullCap = "NullCap"
showCapName UntypedCap {} = "UntypedCap"
showCapName EndpointCap {} = "EndpointCap"
showCapName NotificationCap {} = "AsyncEndpointCap" -- TODO: Rename AsyncEndpointCap to NotificationCap
showCapName ReplyCap {} = "ReplyCap"
showCapName MasterReplyCap {} = "MasterReplyCap"
showCapName CNodeCap {} = "CNodeCap"
showCapName TCBCap {} = "TCBCap"
showCapName IRQControlCap = "IRQControlCap"
showCapName IRQHandlerCap {} = "IRQHandlerCap"
showCapName IRQIOAPICHandlerCap {} = "IRQIOAPICHandlerCap"
showCapName IRQMSIHandlerCap {} = "IRQMSIHandlerCap"
showCapName DomainCap = "DomainCap"
showCapName FrameCap {} = "FrameCap"
showCapName PTCap {} = "PTCap"
showCapName PDCap {} = "PDCap"
showCapName PDPTCap {} = "PDPTCap"
showCapName PML4Cap {} = "PML4Cap"
showCapName ASIDControlCap = "ASIDControlCap"
showCapName ASIDPoolCap {} = "ASIDPoolCap"
showCapName IOPortsCap {} = "IOPortsCap"
showCapName IOSpaceMasterCap = "IOSpaceMasterCap"
showCapName IOSpaceCap {} = "IOSpaceCap"
showCapName ARMIOSpaceCap {} = "ARMIOSpaceCap"
showCapName IOPTCap {} = "IOPTCap"
showCapName VCPUCap {} = "VCPUCap"
showCapName SCCap {} = "SCCap"
showCapName RTReplyCap {} = "RTReplyCap"
showCapName SchedControlCap {} = "SchedControlCap"

showExtraCapAttributes :: Cap -> [(String, String)]
showExtraCapAttributes (EndpointCap _ capBadge _) = [("badge", show capBadge)]
showExtraCapAttributes (NotificationCap _ capBadge _) = [("badge", show capBadge)]
showExtraCapAttributes (CNodeCap _ guard guardSize) =
    [("guard", show guard), ("guardSize", show guardSize)]
showExtraCapAttributes (FrameCap _ _ _ False _) = [("cached", "False")]
showExtraCapAttributes _ = []

--
-- Print the XML for the given cap.
--
printCapXml :: (Word, Cap) -> Doc
printCapXml (location, cap) =
    text (emptyTag "cap" (captype ++ slot ++ target ++ rights ++ attrs))
    where
        slot = [("slot", show location)]
        captype = [("type", showCapName cap)]
        target = if hasObjID cap then [("id", showID (objID cap))] else [("id", "global" ++ showCapName cap)]
        rights = if hasRights cap then [("rights", showRights (capRights cap))] else []
        attrs = showExtraCapAttributes cap

--
-- Print the XML for a CapMap
--
printCapMap :: CapMap Word -> Doc
printCapMap x =
    vcat (map printCapXml (Map.toList x))

--
-- Print an object that just has a name and some simple attributes.
--
printSimpleObject :: KernelObject Word -> ObjID -> [(String, String)] -> Doc
printSimpleObject object objId attrs =
    if hasSlots object then
        (xmlSurround "object" all_attrs $ printCapMap (slots object))
    else
        text (emptyTag "object" all_attrs)
    where
        all_attrs = [("id", showID objId), ("type", showObjectName object)] ++ attrs

--
-- Print the given object.
--
printObject :: (ObjID, KernelObject Word) -> Doc
printObject (objId, object) =
    printSimpleObject object objId (showObjectAttrs object)

-- Print the contents of all objects in the given heap.
printObjects :: ObjMap Word -> Doc
printObjects x =
    xmlSurround "objects" [] $ vcat (map printObject (Map.toList x))

printCovered :: [ObjID] -> Doc
printCovered objs =
    xmlSurround "covered" [] ids
    where ids = vcat $ map text $ map (\obj -> emptyTag "id" [("id", showID obj)]) objs

printCover :: (ObjID, ObjSet) -> Doc
printCover (untyped, cover) =
    xmlSurround "cover" [("untyped", showID untyped)] $ printCovered (Set.toList cover)

printUntypedCovers :: CoverMap -> Doc
printUntypedCovers untypedCovers =
    xmlSurround "untypedCovers" [] $ vcat (map printCover (Map.toList untypedCovers))

printCDTRelation :: (CapRef, CapRef) -> Doc
printCDTRelation (child, parent) =
    text $ emptyTag "cdtRelation" [("child", showCapRef child), ("parent", showCapRef parent)]

printCDT :: CDT -> Doc
printCDT cdt =
    xmlSurround "cdt" [] $ vcat (map printCDTRelation (Map.toList cdt))

-- Print the contents of a model in XML format.
printXml :: String -> Model Word -> Doc
printXml _ (Model arch ms _ cdt untypedCovers) =
    text xml_header
        $+$ (xmlSurround "model" [("arch", show arch)] $ printObjects ms $+$ printUntypedCovers untypedCovers $+$ printCDT cdt)
        $+$ text "\n"
