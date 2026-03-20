--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module CapDL.Parser where

import Prelude ()
import Prelude.Compat
import CapDL.AST
import CapDL.ParserUtils
import Data.Word (Word64)

import Text.ParserCombinators.Parsec

obj_decl_or_ref :: MapParser (Either KODecl NameRef)
obj_decl_or_ref =
        (try $ do
            d <- obj_decl
            comma <|> return ""
            return $ Left d)
    <|> (do n <- name_ref
            comma <|> return ""
            return $ Right n)

opt_obj_decls :: MapParser [Either KODecl NameRef]
opt_obj_decls = braces (many obj_decl_or_ref) <|> return []

object :: MapParser KO
object = do
    typ <- object_type
    params <- object_params
    decls <- opt_obj_decls
    return (Obj typ params decls)

obj_decl :: MapParser KODecl
obj_decl = do
    qname <- qname
    symbol "="
    obj <- CapDL.Parser.object
    return (KODecl qname obj)

obj_decls :: MapParser [Decl]
obj_decls = do
    reserved "objects"
    decls <- braces $ many obj_decl
    return $ map ObjDecl decls

cap_mapping :: Maybe Word -> Maybe NameRef -> MapParser CapMapping
cap_mapping sl nm = do
    obj <- name_ref
    params <- cap_params
    parent <- maybe_parent
    return $ CapMapping sl nm obj params parent

cap_name_ref :: Maybe Word -> Maybe NameRef -> MapParser CapMapping
cap_name_ref sl nm = do
    symbol "<"
    name <- name_ref
    symbol ">"
    params <- cap_params
    parent <- maybe_parent
    return $ CopyOf sl nm name params parent

maybe_name :: MapParser (Maybe NameRef)
maybe_name =
    optionMaybe $ try $ do
        n <- name_ref
        symbol "="
        return n

cap_mapping_or_ref :: MapParser CapMapping
cap_mapping_or_ref = do
    sl <- maybe_slot
    nm <- maybe_name
    cap_mapping sl nm <|> cap_name_ref sl nm

cap_decl :: MapParser Decl
cap_decl = do
    n <- name_ref
    ms <- braces (sepEndBy cap_mapping_or_ref opt_semi)
    return $ CapDecl n ms

cap_name_decl :: MapParser Decl
cap_name_decl = do
    n <- name
    symbol "="
    symbol "("
    ref <- name_ref
    symbol ","
    slot <- parse_slot
    symbol ")"
    return $ CapNameDecl n ref slot

cap_decls :: MapParser [Decl]
cap_decls = do
    reserved "caps"
    braces $ many (try cap_name_decl <|> try cap_decl)

word_pair :: MapParser (Word, Word64)
word_pair =
    parens $ do
        a <- number
        comma
        b <- integer64
        return (a, b)

dom_content :: MapParser DomainDeclItem
dom_content =
    do
        reserved "domain_set_start"
        colon
        maybeStart <- fmap Just number <|> fmap (const Nothing) (reserved "no_start")
        return $ DomStartDecl maybeStart
    <|>
    do
        reserved "schedule"
        colon
        fmap DomScheduleDecl $ brackets $ sepEndBy1 word_pair comma
    <|>
    do
        reserved "index_shift"
        colon
        fmap DomIdxShiftDecl number

dom_decls :: MapParser [Decl]
dom_decls = do
    reserved "domains"
    items <- braces $ many dom_content
    return [DomainDecl items]

decl_section :: MapParser [Decl]
decl_section =
    obj_decls <|> cap_decls <|> irq_decls <|> cdt_decls <|> dom_decls

capDLModule :: MapParser Module
capDLModule = do
    whiteSpace
    arch <- parse_arch
    decls <- many1 decl_section
    eof
    return (Module arch (concat decls))
