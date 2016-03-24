--
-- Copyright 2014, NICTA
--
-- This software may be distributed and modified according to the terms of
-- the BSD 2-Clause license. Note that NO WARRANTY is provided.
-- See "LICENSE_BSD2.txt" for details.
--
-- @TAG(NICTA_BSD)
--

{-# LANGUAGE DeriveDataTypeable #-}
module CapDL.AST where

import CapDL.Model

import Prelude ()
import Prelude.Compat
import Data.Data

-- AST

type Name = String
type QName = [NameRef]

data Range = Only Word
           | FromTo Word Word
           | From Word
           | To Word
           | All
           deriving (Show, Eq, Ord, Typeable, Data)

type NameRef = (Name, [Range])

data TCBExtraParam =
    Addr {
      addr :: Word }
  | IP {
      ip :: Word }
  | SP {
      sp :: Word }
  | Elf {
      elf :: String }
  | Prio {
      prio :: Integer }
   deriving (Show, Eq, Ord, Typeable, Data)


data ObjParam =
    BitSize {
      bits :: Word }
  | VMSize {
      vmSize :: Word }
  | IOPTLevel {
      level :: Word }
  | Paddr {
      paddr :: Word }
  | PortsSize {
      size :: Word }
  | TCBExtraParam {
      extraParam :: TCBExtraParam }
  | InitArguments {
      arguments :: [Word] }
  | DomainID {
      domainID :: Word }
  | PCIDevice {
      pciDevice :: (Word, Word, Word) }
  | Dom {
      dom :: Integer }
  | FaultEP {
      faulEP :: Word }
  deriving (Show, Eq, Ord, Typeable, Data)

data KO = Obj {
    koType :: KOType,
    params :: [ObjParam],
    objDecls :: [Either KODecl NameRef]
} deriving (Show, Eq)

-- obj and slot or declared name
type SlotRef = Either (NameRef, Word) NameRef

data CapParam
    = Rights  {
        rights :: CapRights }
    | Masked {
        rights :: CapRights }
    | Guard {
        guard :: Word }
    | GuardSize {
        guardSize :: Word }
    | IRQRef
    | Badge {
        theBadge :: Word }
    | Range {
        theRange :: [Range] }
    | Reply
    | MasterReply
    | Asid {
        asid :: Asid }
    | Cached {
        cached :: Bool }
    | FrameMapping {
        container :: NameRef,
        slotIndex :: Word }
    deriving (Show, Eq, Ord, Typeable, Data)


data CapMapping
    = CapMapping {
        slot :: Maybe Word,
        capName :: Maybe NameRef,
        objRef :: NameRef,
        capParams :: [CapParam],
        maybeParent :: Maybe SlotRef
      }
    | CopyOf {
        slot :: Maybe Word,
        capName :: Maybe NameRef,
        target :: NameRef,
        copyParams :: [CapParam],
        maybeParent :: Maybe SlotRef
      }
    | IRQMapping {
        slot :: Maybe Word,
        objRef :: NameRef
      }
    | ASIDMapping {
        slot :: Maybe Word,
        objRef :: NameRef
      }
    deriving Show

data KODecl = KODecl {
    objName :: QName,
    object :: KO
} deriving (Show, Eq)

data Decl
    = ObjDecl {
        theKODecl :: KODecl }
    | CapDecl {
        nameRef :: NameRef,
        mappings :: [CapMapping] }
    | CapNameDecl {
        declCapName :: Name,
        declObjRef :: NameRef,
        declSlot :: Word }
    | IRQDecl {
        irqs :: [CapMapping] }
    | ASIDDecl {
        asids :: [CapMapping] }
    | CDTDecl {
        parentRef :: SlotRef,
        children :: [Either Decl SlotRef] }
    deriving Show


data Module = Module {
  theArch :: Arch,
  decls :: [Decl]
} deriving Show

