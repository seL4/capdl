{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module CapDL.PrintJSON
    ( printJSON
    ) where

import Control.Exception (assert)
import Data.Aeson (ToJSON, encode, toJSON, FromJSON, genericToJSON, genericToEncoding, genericParseJSON)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Word (Word8, Word64)
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Set as S

import CapDL.PrintUtils (sortObjects)
import qualified CapDL.Model as C


type Badge = Word
type CPtr = Word

type ObjID = Integer
type CapSlot = Integer
type CapTable = [(CapSlot, Cap)]

data Spec = Spec
    { objects :: [NamedObject]
    , irqs :: [(Word, ObjID)]
    , domain_schedule :: Maybe [DomainSchedEntry]
    , domain_set_start :: Maybe Word
    , domain_idx_shift :: Maybe Word
    , asid_slots :: [ObjID]
    , root_objects :: Range ObjID
    , untyped_covers :: [UntypedCover]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DomainSchedEntry = DomainSchedEntry
    { id :: Word8
    , time :: Word64
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Range a = Range
    { start :: a
    , end :: a
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UntypedCover = UntypedCover
    { parent :: ObjID
    , children :: Range ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NamedObject = NamedObject
    { name :: String
    , object :: Object
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Object =
      Object_Untyped ObjectUntyped
    | Object_Endpoint UnitCompat
    | Object_Notification UnitCompat
    | Object_CNode ObjectCNode
    | Object_Tcb ObjectTCB
    | Object_Irq ObjectIRQ
    | Object_VCpu UnitCompat
    | Object_Frame ObjectFrame
    | Object_PageTable ObjectPageTable
    | Object_AsidPool ObjectASIDPool
    | Object_ArmIrq ObjectArmIRQ
    | Object_IrqMsi ObjectIRQMSI
    | Object_IrqIOApic ObjectIRQIOAPIC
    | Object_IOPorts ObjectIOPorts
    | Object_SchedContext ObjectSchedContext
    | Object_Reply UnitCompat
    deriving (Eq, Show, Generic)

instance ToJSON Object where
    toJSON = genericToJSON $ sumTypeOptions "Object_"
    toEncoding = genericToEncoding $ sumTypeOptions "Object_"

instance FromJSON Object where
    parseJSON = genericParseJSON $ sumTypeOptions "Object_"

data Cap =
      Cap_Untyped CapUntyped
    | Cap_Endpoint CapEndpoint
    | Cap_Notification CapNotification
    | Cap_CNode CapCNode
    | Cap_Tcb CapTCB
    | Cap_IrqHandler CapIRQHandler
    | Cap_VCpu CapVCPU
    | Cap_Frame CapFrame
    | Cap_PageTable CapPageTable
    | Cap_AsidPool CapASIDPool
    | Cap_ArmIrqHandler CapArmIRQHandler
    | Cap_IrqMsiHandler CapIRQMSIHandler
    | Cap_IrqIOApicHandler CapIRQIOAPICHandler
    | Cap_IOPorts CapIOPorts
    | Cap_SchedContext CapSchedContext
    | Cap_Reply CapReply
    deriving (Eq, Show, Generic)

instance ToJSON Cap where
    toJSON = genericToJSON $ sumTypeOptions "Cap_"
    toEncoding = genericToEncoding $ sumTypeOptions "Cap_"

instance FromJSON Cap where
    parseJSON = genericParseJSON $ sumTypeOptions "Cap_"

data Rights = Rights
    { read :: Bool
    , write :: Bool
    , grant :: Bool
    , grant_reply :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Fill = Fill
    { entries :: [FillEntry]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FillEntry = FillEntry
    { range :: FillEntryRange
    , content :: FillEntryContent
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FillEntryRange = FillEntryRange
    { start :: Word
    , end :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FillEntryContent =
      FillEntryContent_Data FillEntryContentFile
    | FillEntryContent_BootInfo FillEntryContentBootInfo
    deriving (Eq, Show, Generic)

instance ToJSON FillEntryContent where
    toJSON = genericToJSON $ sumTypeOptions "FillEntryContent_"
    toEncoding = genericToEncoding $ sumTypeOptions "FillEntryContent_"

instance FromJSON FillEntryContent where
    parseJSON = genericParseJSON $ sumTypeOptions "FillEntryContent_"

data FillEntryContentFile = FillEntryContentFile
    { file :: String
    , file_offset :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FillEntryContentBootInfo = FillEntryContentBootInfo
    { id :: FillEntryContentBootInfoId
    , offset :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FillEntryContentBootInfoId =
      Padding
    | Fdt
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectUntyped = ObjectUntyped
    { size_bits :: Word
    , paddr :: Maybe Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectCNode = ObjectCNode
    { size_bits :: Word
    , slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectTCB = ObjectTCB
    { slots :: CapTable
    , extra :: ObjectTCBExtraInfo
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectTCBExtraInfo = ObjectTCBExtraInfo
    { ipc_buffer_addr :: Word
    , affinity :: Word
    , prio :: Word
    , max_prio :: Word
    , resume :: Bool
    , ip :: Word
    , sp :: Word
    , gprs :: [Word]
    , master_fault_ep :: CPtr
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectIRQ = ObjectIRQ
    { slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectFrame = ObjectFrame
    { size_bits :: Word
    , paddr :: Maybe Word
    , init :: Fill
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectPageTable = ObjectPageTable
    { is_root :: Bool
    , level :: Maybe Int
    , slots :: CapTable
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectASIDPool = ObjectASIDPool
    { high :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectArmIRQ = ObjectArmIRQ
    { slots :: CapTable
    , extra :: ObjectArmIRQExtraInfo
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectArmIRQExtraInfo = ObjectArmIRQExtraInfo
    { trigger :: Word
    , target :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectIRQMSI = ObjectIRQMSI
    { slots :: CapTable
    , extra :: ObjectIRQMSIExtraInfo
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectIRQMSIExtraInfo = ObjectIRQMSIExtraInfo
    { handle:: Word
    , pci_bus :: Word
    , pci_dev :: Word
    , pci_func :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectIRQIOAPIC = ObjectIRQIOAPIC
    { slots :: CapTable
    , extra :: ObjectIRQIOAPICExtraInfo
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectIRQIOAPICExtraInfo = ObjectIRQIOAPICExtraInfo
    { ioapic :: Word
    , pin :: Word
    , level :: Word
    , polarity :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectIOPorts = ObjectIOPorts
    { start_port :: Word
    , end_port :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectSchedContext = ObjectSchedContext
    { size_bits :: Word
    , extra :: ObjectSchedContextExtraInfo
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ObjectSchedContextExtraInfo = ObjectSchedContextExtraInfo
    { period :: Word64
    , budget :: Word64
    , badge :: Badge
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapUntyped = CapUntyped
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapEndpoint = CapEndpoint
    { object :: ObjID
    , badge :: Badge
    , rights :: Rights
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapNotification = CapNotification
    { object :: ObjID
    , badge :: Badge
    , rights :: Rights
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapCNode = CapCNode
    { object :: ObjID
    , guard :: Word
    , guard_size :: Word
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapTCB = CapTCB
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapIRQHandler = CapIRQHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapVCPU = CapVCPU
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapFrame = CapFrame
    { object :: ObjID
    , rights :: Rights
    , cached :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapPageTable = CapPageTable
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapASIDPool = CapASIDPool
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapArmIRQHandler = CapArmIRQHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapIRQMSIHandler = CapIRQMSIHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapIRQIOAPICHandler = CapIRQIOAPICHandler
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapIOPorts = CapIOPorts
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapSchedContext = CapSchedContext
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CapReply = CapReply
    { object :: ObjID
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)


sumTypeOptions :: String -> A.Options
sumTypeOptions prefix = A.defaultOptions
    { A.constructorTagModifier = fromJust . stripPrefix prefix
    , A.sumEncoding = A.ObjectWithSingleField
    }

data UnitCompat = UnitCompat
    deriving (Eq, Show, Generic)

instance ToJSON UnitCompat where
    toJSON UnitCompat = A.Null

instance FromJSON UnitCompat where
    parseJSON _ = pure UnitCompat


printJSON :: C.ObjectSizeMap -> C.Model Word -> String
printJSON = curry $ unpack . encode . uncurry translate

translate :: C.ObjectSizeMap -> C.Model Word -> Spec
translate objSizeMap (C.Model arch objMap irqNode _ coverMap optDomSchedule domStart domIdxShift) = Spec
    { objects
    , irqs
    , domain_schedule = fmap (map (\(id, time) -> DomainSchedEntry
        { id = fromIntegral id
        , time
        })) optDomSchedule
    , domain_set_start = domStart
    , domain_idx_shift = Just domIdxShift
    , asid_slots = asidSlots
    , root_objects = Range 0 (toInteger numRootObjects)
    , untyped_covers = untypedCovers
    }
  where
    orderedObjectIds = rootObjectIds ++ childObjectIds

    numRootObjects = length rootObjectIds

    rootObjectIds = map fst sorted
      where
        allChildren = S.fromList . concat $ M.elems coverMap
        unsorted = filter (`S.notMember` allChildren) (M.keys objMap)
        sorted = sortObjects objSizeMap [ (objId, objMap M.! objId) | objId <- unsorted ]

    (_, childObjectIds, untypedCovers) =
        foldr f (numRootObjects, [], []) (concatMap M.toList (objectLayers coverMap))
      where
        f (parent, children) (n, allChildren, covers) =
            ( n + length children
            , allChildren ++ children
            , covers ++ [UntypedCover (translateId parent) (Range (toInteger n) (toInteger (n + length children)))]
            )

    translateId = (M.!) (M.fromList (zip orderedObjectIds [0..]))
    translateCapTable = M.toList . M.map translateCap . M.mapKeys toInteger

    pageTableIsVSpace = flip S.member . S.fromList $ do
        C.TCB { slots } <- M.elems objMap
        C.PTCap { capObj } <- return $ slots M.! C.tcbVTableSlot
        return capObj

    irqs =
        [ (irq, translateId obj)
        | (irq, obj) <- M.toAscList irqNode
        ]

    asidSlots = assert (map fst table `isPrefixOf` [1..]) (map snd table)
      where
        table = sortBy (comparing fst)
            [ let Just asidHigh = maybeAsidHigh
              in assert (M.null lowSlots) (asidHigh, translateId objID)
            | (objID, C.ASIDPool lowSlots maybeAsidHigh) <- M.toList objMap
            ]

    objects =
        [ NamedObject
            { name = translateName objId
            , object = translateObj objId
            }
        | objId <- orderedObjectIds
        ]

    translateObj objId = case objMap M.! objId of
        C.Untyped { maybeSizeBits = Just sizeBits, maybePaddr } -> Object_Untyped $ ObjectUntyped
            { size_bits = sizeBits
            , paddr = maybePaddr
            }
        C.Endpoint -> Object_Endpoint UnitCompat
        C.Notification -> Object_Notification UnitCompat
        C.Frame { vmSizeBits, maybePaddr, maybeFill } -> Object_Frame $ ObjectFrame
            { size_bits = vmSizeBits
            , paddr = maybePaddr
            , init = translateFill maybeFill
            }
        C.PT slots -> Object_PageTable $
            let translatedSlots = translateCapTable slots
            in case arch of
                C.RISCV -> ObjectPageTable
                    { is_root = pageTableIsVSpace objId
                    , level = Nothing
                    , slots = translatedSlots
                    }
                _ -> ObjectPageTable
                    { is_root = False
                    , level = Just 3
                    , slots = translatedSlots
                    }
        C.PD slots -> Object_PageTable $
            let translatedSlots = translateCapTable slots
            in case arch of
                C.ARM11 -> ObjectPageTable
                    { is_root = True
                    , level = Just 0
                    , slots = translatedSlots
                    }
                _ -> ObjectPageTable
                    { is_root = False
                    , level = Just 2
                    , slots = translatedSlots
                    }
        C.PUD slots -> Object_PageTable $ ObjectPageTable
            { is_root = False
            , level = Just 1
            , slots = translateCapTable slots
            }
        C.PGD slots -> Object_PageTable $ ObjectPageTable
            { is_root = True
            , level = Just 0
            , slots = translateCapTable slots
            }
        C.PDPT slots -> Object_PageTable $ ObjectPageTable
            { is_root = False
            , level = Just 1
            , slots = translateCapTable slots
            }
        C.PML4 slots -> Object_PageTable $ ObjectPageTable
            { is_root = True
            , level = Just 0
            , slots = translateCapTable slots
            }
        -- model uses 0-sized CNodes as token objects for IRQs
        C.CNode slots 0 -> Object_Irq $ ObjectIRQ
            { slots = translateCapTable slots
            }
        C.CNode slots sizeBits -> Object_CNode $ ObjectCNode
            { size_bits = sizeBits
            , slots = translateCapTable slots
            }
        C.VCPU -> Object_VCpu UnitCompat
        C.ARMIrq slots trigger target -> Object_ArmIrq $ ObjectArmIRQ
            { slots = translateCapTable slots
            , extra = ObjectArmIRQExtraInfo trigger target
            }
        C.MSIIrq slots handle bus dev fun -> Object_IrqMsi $ ObjectIRQMSI
            { slots = translateCapTable slots
            , extra = ObjectIRQMSIExtraInfo handle bus dev fun
            }
        C.IOAPICIrq slots ioapic pin ioapic_level polarity -> Object_IrqIOApic $ ObjectIRQIOAPIC
            { slots = translateCapTable slots
            , extra = ObjectIRQIOAPICExtraInfo ioapic pin ioapic_level polarity
            }
        C.IOPorts (start_port, end_port) -> Object_IOPorts $ ObjectIOPorts
            { start_port
            , end_port
            }
        C.ASIDPool slots (Just asidHigh) -> assert (M.null slots) Object_AsidPool $ ObjectASIDPool asidHigh
        C.RTReply -> Object_Reply UnitCompat
        C.TCB
            { slots
            , faultEndpoint
            , extraInfo = Just extraInfo
            , initArguments
            } ->
            let C.TCBExtraInfo
                    { ipcBufferAddr
                    , ip = Just ip
                    , sp = Just sp
                    , prio = Just prio
                    , max_prio = Just max_prio
                    , affin = Just affinity
                    , resume
                    } = extraInfo
            in Object_Tcb (ObjectTCB
                { slots = translateCapTable slots
                , extra = ObjectTCBExtraInfo
                    { ipc_buffer_addr = ipcBufferAddr
                    , affinity = fromIntegral affinity
                    , prio = fromIntegral prio
                    , max_prio = fromIntegral max_prio
                    , resume = fromMaybe True resume
                    , ip
                    , sp
                    , gprs = initArguments
                    , master_fault_ep = fromMaybe 0 faultEndpoint
                    }
                })
        C.SC
            { maybeSizeBits = Just sizeBits
            , sc_extraInfo = Just extraInfo
            } ->
            let C.SCExtraInfo
                    { period = Just period
                    , budget = Just budget
                    , scData = Just badge
                    } = extraInfo
            in Object_SchedContext (ObjectSchedContext
                { size_bits = sizeBits
                , extra = ObjectSchedContextExtraInfo
                    { period
                    , budget
                    , badge
                    }
                })
        x -> traceShow x undefined

    translateCap cap = case cap of
        C.UntypedCap capObj -> Cap_Untyped $ CapUntyped (translateId capObj)
        C.EndpointCap capObj capBadge capRights -> Cap_Endpoint $ CapEndpoint
            { object = translateId capObj
            , badge = capBadge
            , rights = translateRights capRights
            }
        C.NotificationCap capObj capBadge capRights -> Cap_Notification $ CapNotification
            { object = translateId capObj
            , badge = capBadge
            , rights = translateRights capRights
            }
        C.CNodeCap capObj capGuard capGuardSize -> Cap_CNode $ CapCNode
            { object = translateId capObj
            , guard = capGuard
            , guard_size = capGuardSize
            }
        C.TCBCap capObj -> Cap_Tcb $ CapTCB (translateId capObj)
        C.IRQHandlerCap capObj -> Cap_IrqHandler $ CapIRQHandler (translateId capObj)
        C.VCPUCap capObj -> Cap_VCpu $ CapVCPU (translateId capObj)
        C.FrameCap { capObj, capRights, capCached } -> Cap_Frame $ CapFrame
            { object = translateId capObj
            , rights = translateRights capRights
            , cached = capCached
            }
        C.PTCap capObj _ -> Cap_PageTable $ CapPageTable (translateId capObj)
        C.PDCap capObj _ -> Cap_PageTable $ CapPageTable (translateId capObj)
        C.PUDCap capObj _ -> Cap_PageTable $ CapPageTable (translateId capObj)
        C.PGDCap capObj _ -> Cap_PageTable $ CapPageTable (translateId capObj)
        C.PDPTCap capObj _ -> Cap_PageTable $ CapPageTable (translateId capObj)
        C.PML4Cap capObj _ -> Cap_PageTable $ CapPageTable (translateId capObj)
        C.ARMIRQHandlerCap capObj -> Cap_ArmIrqHandler $ CapArmIRQHandler (translateId capObj)
        C.IRQMSIHandlerCap capObj -> Cap_IrqMsiHandler $ CapIRQMSIHandler (translateId capObj)
        C.IRQIOAPICHandlerCap capObj -> Cap_IrqIOApicHandler $ CapIRQIOAPICHandler (translateId capObj)
        C.IOPortsCap capObj -> Cap_IOPorts $ CapIOPorts (translateId capObj)
        C.ASIDPoolCap capObj -> Cap_AsidPool $ CapASIDPool (translateId capObj)
        C.SCCap capObj -> Cap_SchedContext $ CapSchedContext (translateId capObj)
        C.RTReplyCap capObj -> Cap_Reply $ CapReply (translateId capObj)

translateName :: C.ObjID -> String
translateName (name, Nothing) = name

translateFill :: Maybe [[String]] -> Fill
translateFill = Fill . map f . concat . toList
  where
    f (dest_offset:dest_len:rest) = FillEntry
        { range = FillEntryRange { start, end }
        , content
        }
      where
        start = read dest_offset
        len = read dest_len
        end = start + len
        content = case rest of
            ["CDL_FrameFill_FileData", file, file_offset] -> FillEntryContent_Data
                (FillEntryContentFile
                    { file = tail (Data.List.init file)
                    , file_offset = read file_offset
                    })
            ["CDL_FrameFill_BootInfo", id, offset] -> FillEntryContent_BootInfo
                (FillEntryContentBootInfo
                    { id = case id of
                        "CDL_FrameFill_BootInfo_FDT" -> Fdt
                    , offset = read offset
                    })

translateRights :: C.CapRights -> Rights
translateRights = foldr f noRights
    where
    f right acc = case right of
        C.Read -> acc { read = True }
        C.Write -> acc { write = True }
        C.Grant -> acc { grant = True }
        C.GrantReply -> acc { grant_reply = True }

noRights :: Rights
noRights = Rights False False False False

objectLayers :: C.CoverMap -> [C.CoverMap]
objectLayers = unfoldr $ \intermediate ->
    if M.null intermediate
    then Nothing
    else
        let children = S.fromList . concat $ M.elems intermediate
        in Just $ M.partitionWithKey (const . not . (`S.member` children)) intermediate
