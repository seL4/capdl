--
-- Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
--
-- SPDX-License-Identifier: BSD-2-Clause
--

{-# LANGUAGE DeriveDataTypeable #-}
module CapDL.AST where

import CapDL.Model

import Prelude ()
import Prelude.Compat
import Data.Data
import Data.Word

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
  | Prio {
      prio :: Integer }
  | MaxPrio {
      max_prio :: Integer }
  | Affinity {
      affinity :: Integer }
  | Resume {
      resume :: Bool }
   deriving (Show, Eq, Ord, Typeable, Data)

data FrameExtraParam =
    Fill { fill :: [[String]] }
  deriving (Show, Eq, Ord, Typeable, Data)

data SCExtraParam =
    Period {
      period :: Word64 }
  | Budget {
      budget :: Word64 }
  | SCData {
      scData :: Word }
   deriving (Show, Eq, Ord, Typeable, Data)

data CBExtraParam = 
    CBNumber {
      cbNumber :: Word }
    deriving (Eq, Show, Ord, Typeable, Data)

data IOAPICIRQExtraParam =
    IOAPIC {
        ioapic :: Word }
  | Pin {
        pin :: Word }
  | Level {
        ioapic_level :: Word }
  | Polarity {
        polarity :: Word }
    deriving (Show, Eq, Ord, Typeable, Data)

data MSIIRQExtraParam =
    MSIHandle {
        handle :: Word }
  | MSIPCIBus {
        bus :: Word }
  | MSIPCIDev {
        dev :: Word }
  | MSIPCIFun {
        fun :: Word }
    deriving (Show, Eq, Ord, Typeable, Data)

data ARMIRQExtraParam =
    ARMIRQTrigger {
        trigger :: Word }
  | ARMIRQTarget {
        target_core :: Word }
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
  | TCBExtraParam {
      extraParam :: TCBExtraParam }
  | FrameExtraParam {
      frameExtraParam :: FrameExtraParam}
  | SCExtraParam {
      sc_extraParam :: SCExtraParam }
  | CBExtraParam {
      cb_extraParam :: CBExtraParam }
  | IOAPICIRQExtraParam {
      ioapic_irq_extraParam :: IOAPICIRQExtraParam }
  | MSIIRQExtraParam {
      msi_irq_extraParam :: MSIIRQExtraParam }
  | ARMIRQExtraParam {
      arm_irq_extraParam :: ARMIRQExtraParam }
  | InitArguments {
      arguments :: [Word] }
  | DomainID {
      domainID :: Word }
  | ARMIOSpace {
      armiospace :: Word }
  | PCIDevice {
      pciDevice :: (Word, Word, Word) }
  | Dom {
      dom :: Integer }
  | FaultEP {
      faulEP :: Word }
  | Ports {
      theRange :: (Word, Word) }
  | AsidHigh {
      asidHigh :: Word }
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
    | Core {
        theCore :: Word }
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
