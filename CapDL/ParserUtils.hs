--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

module CapDL.ParserUtils where

import CapDL.AST
import CapDL.Model

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language

import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (find)
import Numeric

data Maps = Maps {
    refMap :: Map.Map Name ObjID,
    addrMap :: Map.Map ObjID (Word, Word),
    covMap :: Map.Map ObjID (Set.Set ObjID),
    irqMap :: Map.Map Word Name
}

emptyMaps :: Maps
emptyMaps = Maps Map.empty Map.empty Map.empty Map.empty

type MapParser = GenParser Char Maps

capDL_Lang = emptyDef {
    commentStart = "/*",
    commentEnd = "*/",
    commentLine = "--",
    nestedComments = True,
    identStart = letter,
    identLetter = alphaNum <|> oneOf "-_@",
    opStart = opLetter capDL_Lang,
    opLetter = oneOf ":=",
    reservedNames = ["objects", "caps"],
    caseSensitive = True
}

lexer :: PT.TokenParser Maps
lexer  = PT.makeTokenParser capDL_Lang

whiteSpace= PT.whiteSpace lexer
lexeme    = PT.lexeme lexer
symbol    = PT.symbol lexer
natural   = PT.natural lexer
parens    = PT.parens lexer
braces    = PT.braces lexer
brackets  = PT.brackets lexer
semi      = PT.semi lexer
dot       = PT.dot lexer
comma     = PT.comma lexer
colon     = PT.colon lexer
identifier= PT.identifier lexer
reserved  = PT.reserved lexer

name :: MapParser Name
name = identifier

number :: MapParser Word
number = do
    n <- natural
    return (fromIntegral n)

integer :: MapParser Integer
integer = do
    n <- natural
    return $ fromIntegral n

name_ref :: MapParser NameRef
name_ref = do
    n <- name
    rs <- ranges
    return (n,rs)

qname :: MapParser QName
qname = sepBy1 name_ref dot

keyw :: String -> a -> MapParser a
keyw st v = do
    reserved st
    return v

parse_either :: MapParser a -> MapParser b -> MapParser (Either a b)
parse_either a b =
        do x <- a
           return $ Left x
    <|> do x <- b
           return $ Right x

parse_arch :: MapParser Arch
parse_arch = do
    reserved "arch"
    keyw "ia32" IA32 <|> keyw "arm11" ARM11

object_type :: MapParser KOType
object_type =
        keyw "ep" Endpoint_T
    <|> keyw "aep" AsyncEndpoint_T
    <|> keyw "tcb" TCB_T
    <|> keyw "cnode" CNode_T
    <|> keyw "ut" Untyped_T
    <|> keyw "irq" IrqSlot_T
    <|> keyw "asid_pool" ASIDPool_T
    <|> keyw "pt" PT_T
    <|> keyw "pd" PD_T
    <|> keyw "frame" Frame_T
    <|> keyw "io_ports" IOPorts_T
    <|> keyw "io_device" IODevice_T
    <|> keyw "io_pt" IOPT_T
    <|> keyw "vcpu" VCPU_T

obj_bit_size :: MapParser ObjParam
obj_bit_size = do
    n <- number
    reserved "bits"
    return $ BitSize n

obj_vm_type :: Word -> MapParser ObjParam
obj_vm_type n =
        keyw "k"  (VMSize (n * 2^10))
    <|> keyw "M"  (VMSize (n * 2^20))

obj_vm_size :: MapParser ObjParam
obj_vm_size = do
        n <- number
        obj_vm_type n

obj_paddr :: MapParser ObjParam
obj_paddr = do
    reserved "paddr"
    colon
    n <- number
    return $ Paddr n

io_pt_level :: MapParser ObjParam
io_pt_level = do
    reserved "level"
    colon
    l <- number
    return $ IOPTLevel l

obj_ports_size :: MapParser ObjParam
obj_ports_size = do
    n <- number
    reserved "k"
    reserved "ports"
    return $ PortsSize (n * 2^10)

tcb_addr :: MapParser TCBExtraParam
tcb_addr = do
    reserved "addr"
    colon
    n <- number
    return $ Addr n

tcb_ip :: MapParser TCBExtraParam
tcb_ip = do
    reserved "ip"
    colon
    n <- number
    return $ IP n

tcb_sp :: MapParser TCBExtraParam
tcb_sp = do
    reserved "sp"
    colon
    n <- number
    return $ SP n

tcb_elf :: MapParser TCBExtraParam
tcb_elf = do
    reserved "elf"
    colon
    n <- name
    return $ Elf n

tcb_prio :: MapParser TCBExtraParam
tcb_prio = do
    reserved "prio"
    colon
    n <- integer
    return $ Prio n

tcb_extra_param :: MapParser ObjParam
tcb_extra_param = do
    param <-   (tcb_addr
            <|> tcb_ip
            <|> tcb_sp
            <|> tcb_elf
            <|> tcb_prio)
    return $ TCBExtraParam param

tcb_dom :: MapParser ObjParam
tcb_dom = do
    reserved "dom"
    colon
    n <- integer
    return $ Dom n

tcb_fault_ep :: MapParser ObjParam
tcb_fault_ep = do
    reserved "fault_ep"
    colon
    n <- number
    return $ FaultEP n

init_arguments :: MapParser ObjParam
init_arguments = do
    reserved "init"
    colon
    list <- brackets (sepBy number comma)
    return $ InitArguments list

domain_id :: MapParser ObjParam
domain_id = do
    reserved "domainID"
    colon
    dom <- number
    return $ DomainID dom

pci_device :: MapParser ObjParam
pci_device = do
    pci_bus <- number
    colon
    pci_dev <- number
    dot
    pci_fun <- number
    return $ PCIDevice (pci_bus, pci_dev, pci_fun)

object_param :: MapParser ObjParam
object_param =
        try obj_bit_size
    <|> try obj_ports_size
    <|> try obj_vm_size
    <|> io_pt_level
    <|> tcb_extra_param
    <|> tcb_dom
    <|> tcb_fault_ep
    <|> init_arguments
    <|> obj_paddr
    <|> domain_id
    <|> pci_device

object_params :: MapParser [ObjParam]
object_params =
        parens (sepBy object_param comma)
    <|> return []

symbolic_slot :: MapParser Word
symbolic_slot =
        keyw "cspace" tcbCTableSlot
    <|> keyw "vspace" tcbVTableSlot
    <|> keyw "reply_slot"  tcbReplySlot
    <|> keyw "caller_slot" tcbCallerSlot
    <|> keyw "ipc_buffer_slot" tcbIPCBufferSlot
    <|> keyw "fault_ep_slot" tcbFaultEPSlot

parse_slot :: MapParser Word
parse_slot = number <|> symbolic_slot

maybe_slot :: MapParser (Maybe Word)
maybe_slot =
    do  n <- parse_slot
        colon
        return $ Just n
    <|> return Nothing

chr :: Char -> a -> MapParser a
chr c v = lexeme (char c) >> return v

right :: MapParser Rights
right = chr 'R' Read <|> chr 'W' Write <|> chr 'G' Grant <|> chr 'X' Grant

parse_rights :: MapParser CapRights
parse_rights = do
  rs <- many1 right
  return $ Set.fromList rs

range :: MapParser Range
range =
        (try $ do
            a <- number
            symbol ".."
            b <- number
            return $ FromTo a b)
    <|> (try $ do
            symbol ".."
            b <- number
            return $ To b)
    <|> (try $ do
            a <- number
            symbol ".."
            return $ From a)
    <|> (try $ do
            n <- number
            return $ Only n)

ranges :: MapParser [Range]
ranges =
        (try (symbol "[" >> symbol "]" >> return [All]))
    <|> brackets (sepBy1 range comma)
    <|> return []

mask :: MapParser String
mask = do
    reserved "mask"
    colon
    r <- parse_rights
    return ""

parse_asid :: MapParser Asid
parse_asid = do
    symbol "("
    high <- number
    symbol ","
    low <- number
    symbol ")"
    return (high, low)

cap_param :: MapParser CapParam
cap_param =
        do
        reserved "masked"
        colon
        r <- parse_rights
        return $ Masked r
    <|> do
        r <- parse_rights
        return $ Rights r
    <|> do
        reserved "guard"
        colon
        n <- number
        return $ Guard n
    <|> do
        reserved "guard_size"
        colon
        n <- number
        return $ GuardSize n
    <|> do
        reserved "badge"
        colon
        n <- number
        return $ Badge n
    <|> do
        reserved "ports"
        colon
        r <- ranges
        return $ Range r
    <|> do
        reserved "reply"
        return Reply
    <|> do
        reserved "master_reply"
        return MasterReply
    <|> do
        reserved "asid"
        colon
        asid <- parse_asid
        return $ Asid asid
    <|> do
        reserved "cached"
        return $ Cached True
    <|> do
        reserved "uncached"
        return $ Cached False

cap_params :: MapParser [CapParam]
cap_params =
    parens (sepBy1 cap_param comma) <|> return []

opt_semi :: MapParser String
opt_semi = semi <|> return ""

irq_mapping :: MapParser CapMapping
irq_mapping = do
    sl <- maybe_slot
    obj <- name_ref
    return $ IRQMapping sl obj

irq_decl :: MapParser Decl
irq_decl = do
    ms <- sepEndBy irq_mapping opt_semi
    return $ IRQDecl ms

irq_decls :: MapParser [Decl]
irq_decls = do
    reserved "irq maps"
    irqs <- braces (irq_decl)
    return [irqs]

cap_ref :: MapParser (NameRef, Word)
cap_ref = do
    symbol "("
    obj <- name_ref
    comma
    slot <- parse_slot
    symbol ")"
    return (obj, slot)

slot_ref :: MapParser SlotRef
slot_ref =
        (do
            capRef <- cap_ref
            return $ Left capRef)
    <|> (do
            name <- name_ref
            return $ Right name)

maybe_parent :: MapParser (Maybe SlotRef)
maybe_parent =
    optionMaybe $ try $ do
        reserved "- child_of"
        slot_ref

cdt_decl_or_slot_ref :: MapParser (Either Decl SlotRef)
cdt_decl_or_slot_ref =
        (do
            slotRef <- slot_ref
            return $ Right slotRef)
    <|> (do
            cdtDecl <- cdt_decl
            return $ Left cdtDecl)

cdt_decl :: MapParser Decl
cdt_decl = do
    capRef <- slot_ref
    children <- braces (sepEndBy cdt_decl_or_slot_ref opt_semi)
    return $ CDTDecl capRef children

cdt_decls :: MapParser [Decl]
cdt_decls = do
    reserved "cdt"
    braces $ many (try cdt_decl)
