{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Network.OpenFlow.Ofp10
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  experimental
-- Portability :  unknown
--
--------------------------------------------------------------------------------
module Network.OpenFlow.Ofp10
  (
  -- * Messages
    OfpFrame(..)
  , getOfpFrame
  , putOfpFrame
  , OfpHeader(..)
  , getOfpHeader
  , OfpType(..)
  , OfpMessage(..)
  , OfpCapabilities(..)
  , OfpConfigFlags(..)
  , OfpFlowModCommand(..)
  , OfpFlowModFlags(..)
  , OfpStatsTypes(..)
  , OfpRequestStats(..)
  , OfpReplyStats(..)
  , OfpPacketInReason(..)
  , OfpFlowRemovedReason(..)
  , OfpPortReason(..)
  , OfpError(..)
  , OfpHelloFailedCode(..)
  , OfpBadRequestCode(..)
  , OfpBadActionCode(..)
  , OfpFlowModFailedCode(..)
  , OfpPortModFailedCode(..)
  , OfpQueueOpFailedCode(..)

  -- * Flows
  , OfpMatch(..)
  , OfpFlowWildcards(..)
  , OfpActionType(..)
  , OfpAction(..)

  -- * Ports
  , OfpPhyPort(..)
  , OfpPortConfig(..)
  , OfpPortState(..)
  , OfpPort
  , OfpPortFeatures(..)

  -- * Queues
  , OfpPacketQueue(..)
  , OfpQueueProperties(..)
  , OfpQueueProp(..)

  ) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Maybe
import Data.Tuple
import Network.MAC

#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif

data OfpFrame = OfpFrame
  { header  :: !OfpHeader
  , message :: !OfpMessage
  } deriving (Eq, Show)

getOfpFrame :: Get OfpFrame
getOfpFrame = do
  hdr@(OfpHeader _ t l _) <- getOfpHeader
  msg <- getOfpMessage t l
  return $! OfpFrame hdr msg

putOfpFrame :: OfpFrame -> Put
putOfpFrame (OfpFrame h m) = putOfpHeader h >> putOfpMessage m


data OfpHeader = OfpHeader
  { version :: !Word8
  , ty      :: !OfpType
  , len     :: !Word16
  , xid     :: !Word32
  } deriving (Eq, Show)

getOfpHeader :: Get OfpHeader
getOfpHeader = OfpHeader <$> getWord8 <*> get <*> getWord16be <*> getWord32be

putOfpHeader :: OfpHeader -> Put
putOfpHeader (OfpHeader v t l x) = putWord8 v >> put t >> putWord16be l >> putWord32be x


data OfpType =
    OfptHello
  | OfptError
  | OfptEchoRequest
  | OfptEchoReply
  | OfptVendor
  | OfptFeaturesRequest
  | OfptFeaturesReply
  | OfptGetConfigRequest
  | OfptGetConfigReply
  | OfptSetConfig
  | OfptPacketIn
  | OfptFlowRemoved
  | OfptPortStatus
  | OfptPacketOut
  | OfptFlowMod
  | OfptPortMod
  | OfptStatsRequest
  | OfptStatsReply
  | OfptBarrierRequest
  | OfptBarrierReply
  | OfptQueueGetConfigRequest
  | OfptQueueGetConfigReply
  deriving (Enum, Eq, Show)

instance Binary OfpType where
  put = putWord8 . fromIntegral . fromEnum
  get = liftM (toEnum . fromIntegral) getWord8


-- | Enumeration of messages
data OfpMessage =
    OfpSwitchFeatures {
        sfDatapathId   :: !Word64
      , sfNBuffers     :: !Word32
      , sfNTables      :: !Word8
      , sfCapabilities :: [OfpCapabilities]
      , sfActionTypes  :: [OfpActionType]
      , sfPorts        :: [OfpPhyPort]
      }
  | OfpSwitchConfig {
        scFlags        :: [OfpConfigFlags]
      , scMissSendLen  :: !Word16
      }
  | OfpFlowMod {
        fmMatch        :: !OfpMatch
      , fmCookie       :: !Word64
      , fmCommand      :: !OfpFlowModCommand
      , fmIdleTimeout  :: !Word16
      , fmHardTimeout  :: !Word16
      , fmPriority     :: !Word16
      , fmBufferId     :: !Word32
      , fmOutPort      :: !Word16
      , fmFlags        :: !OfpFlowModFlags
      , fmActions      :: [OfpAction]
      }
  | OfpPortMod {
        pmPortNo       :: !Word16
      , pmHwAddr       :: !MAC
      , pmConfig       :: [OfpPortConfig]
      , pmMask         :: [OfpPortConfig]
      , pmAdvertise    :: [OfpPortFeatures]
      }
  | OfpQueueGetConfigRequest {
        qgcReqPort     :: !Word16
      }
  | OfpQueueGetConfigReply {
        qgcRepPort     :: !Word16
      , qgcRepQueues   :: [OfpPacketQueue]
      }
  | OfpStatsRequest {
        sReqTy         :: !OfpStatsTypes
      , sReqFlags      :: () -- ^ FIXME: undefined??
      , sReqBody       :: !OfpRequestStats
      }
  | OfpStatsReply {
        sRepTy         :: !OfpStatsTypes
      , sRepFlags      :: () -- ^ FIXME: undefined??
      , sRepBody       :: [OfpReplyStats]
      }
  | OfpPacketOut {
        poBufferId     :: !Word32
      , poInPort       :: !Word16
      , poActionsLen   :: !Word16
      , poActions      :: [OfpAction]
      , poPacket       :: [Word8]
      }
  | OfpPacketIn {
        piBufferId     :: !Word32
      , piTotalLen     :: !Word16
      , piInPort       :: !Word16
      , piReason       :: !OfpPacketInReason
      , piPacket       :: [Word8]
      }
  | OfpFlowRemoved {
        frMatch        :: !OfpMatch
      , frCookie       :: !Word64
      , frPriority     :: !Word16
      , frReason       :: !OfpFlowRemovedReason
      , frDurationSec  :: !Word32
      , frDurationNsec :: !Word32
      , frIdleTimeout  :: !Word16
      , frPacketCount  :: !Word64
      , frByteCount    :: !Word64
      }
  | OfpPortStatus {
        psReason       :: !OfpPortReason
      , psDesc         :: !OfpPhyPort
      }
  | OfpErrorMsg {
        eError :: !OfpError
      , eData  :: [Word8]
      }
  | OfpVendorHeader {
        vVendor :: !Word32
      , vData   :: [Word8]
      }
  | Body [Word8]
  deriving (Eq, Show)

-- TODO
getOfpMessage :: OfpType -> Word16 -> Get OfpMessage
getOfpMessage t l = return $! Body []

-- TODO
putOfpMessage :: OfpMessage -> Put
putOfpMessage _ = putWord8 0


data OfpCapabilities =
    OfpcFlowStats
  | OfpcTableStats
  | OfpcPortStats
  | OfpcStp
  | OfpcReserved
  | OfpcIpReasm
  | OfpcQueueStats
  | OfpcArpMatchIp
  deriving (Enum, Eq, Show)


data OfpConfigFlags =
    OfpcFragNormal
  | OfpcFragDrop
  | OfpcFragReasm
  | OfpcMask
  deriving (Enum, Eq, Show)


data OfpFlowModCommand =
    OfpfcAdd
  | OfpfcModify
  | OfpfcModifyStrict
  | OfpfcDelete
  | OfpfcDeleteStrict
  deriving (Enum, Eq, Show)


data OfpFlowModFlags =
    OfpffSendFlowRem
  | OfpffCheckOverlap
  | OfpffEmerg
  deriving (Eq, Show)

ofpffTable :: [(OfpFlowModFlags, Int)]
ofpffTable = [
    (OfpffSendFlowRem,  1 `shiftL` 0)
  , (OfpffCheckOverlap, 1 `shiftL` 1)
  , (OfpffEmerg,        1 `shiftL` 2)
  ]

instance Enum OfpFlowModFlags where
  fromEnum = fromJust . flip lookup ofpffTable
  toEnum = fromJust . flip lookup (map swap ofpffTable)


data OfpStatsTypes =
    OfpstDesc       -- ^ () / OfpDescStats
  | OfpstFlow       -- ^ OfpFlowStatsRequest / [OfpFlowStats]
  | OfpstAggregate  -- ^ OfpAggregateStatsRequest / OfpAggregateStatsReply
  | OfpstTable      -- ^ () / [OfpTableStats]
  | OfpstPort       -- ^ OfpPortStatsRequest / [OfpPortStats]
  | OfpstQueue      -- ^ OfpQueueStatsRequest / [OfpQueueStats]
  | OfpstVendor     -- ^ () / ()  FIXME: fromEnum ==> 0xffff
  deriving (Enum, Eq, Show)

data OfpRequestStats =
    OfpFlowStatsRequest {
        fsrMatch   :: !OfpMatch
      , fsrTableId :: !Word8
      , fsrOutPort :: !Word16
      }
  | OfpAggregateStatsRequest {
        asReqMatch   :: !OfpMatch
      , asReqTableId :: !Word8
      , asReqOutPort :: !Word16
      }
  | OfpPortStatsRequest {
        psReqPortNo :: !Word16
      }
  | OfpQueueStatsRequest {
        qsReqPortNo  :: !Word16
      , qsReqQueueId :: !Word32
      }
  deriving (Eq, Show)

data OfpReplyStats =
    OfpDescStats {
        dsMfrDesc   :: [Word8]
      , dsHwDesc    :: [Word8]
      , dsSwDesc    :: [Word8]
      , dsSerialNum :: [Word8]
      , dsDpDesc    :: [Word8]
      }
  | OfpFlowStats {
        fsLength       :: !Word16
      , fsTableId      :: !Word8
      , fsMatch        :: !OfpMatch
      , fsDurationSec  :: !Word32
      , fsDurationNsec :: !Word32
      , fsPriority     :: !Word16
      , fsIdleTimeout  :: !Word16
      , fsHardTimeout  :: !Word16
      , fsCookie       :: !Word64
      , fsPacketCount  :: !Word64
      , fsByteCount    :: !Word64
      , fsActions      :: [OfpAction]
      }
  | OfpAggregateStatsReply {
        asRepPacketCount :: !Word64
      , asRepByteCount   :: !Word64
      , asRepFlowCount   :: !Word32
      }
  | OfpTableStats {
        tsTableId      :: !Word8
      , tsName         :: [Word8]
      , tsWildcards    :: [OfpFlowWildcards]
      , tsMaxEntries   :: !Word32
      , tsActiveCount  :: !Word32
      , tsLookupCount  :: !Word64
      , tsMatchedCount :: !Word64
      }
  | OfpPortStats {
        psPortNo     :: !Word16
      , psRxPackets  :: !Word64
      , psTxPackets  :: !Word64
      , psRxBytes    :: !Word64
      , psTxBytes    :: !Word64
      , psRxDropped  :: !Word64
      , psTxDropped  :: !Word64
      , psRxErrors   :: !Word64
      , psTxErrors   :: !Word64
      , psRxFrameErr :: !Word64
      , psRxOverErr  :: !Word64
      , psRxCrcErr   :: !Word64
      , psCollisions :: !Word64
      }
  | OfpQueueStats {
        qsPortNo    :: !Word16
      , qsQueueId   :: !Word32
      , qsTxBytes   :: !Word64
      , qsTxPackets :: !Word64
      , qsTxErrors  :: !Word64
      }
  deriving (Eq, Show)


data OfpPacketInReason =
    OfprNoMatch
  | OfprAction
  deriving (Enum, Eq, Show)


data OfpFlowRemovedReason =
    OfprrIdleTimeout
  | OfprrHardTimeout
  | OfprrDelete
  deriving (Enum, Eq, Show)


data OfpPortReason =
    OfpprAdd
  | OfpprDelete
  | OfpprModify
  deriving (Enum, Eq, Show)


data OfpError =
    OfpetHelloFailed OfpHelloFailedCode
  | OfpetBadRequest OfpBadRequestCode
  | OfpetBadAction OfpBadActionCode
  | OfpetFlowModFailed OfpFlowModFailedCode
  | OfpetPortModFailed OfpPortModFailedCode
  | OfpetQueueOpFailed OfpQueueOpFailedCode
  deriving (Eq, Show)

data OfpHelloFailedCode =
    OfphfcIncompatible
  | OfphfcEperm
  deriving (Enum, Eq, Show)

data OfpBadRequestCode =
    OfpbrcBadVersion
  | OfpbrcBadType
  | OfpbrcBadStat
  | OfpbrcBadVendor
  | OfpbrcBadSubtype
  | OfpbrcEperm
  | OfpbrcBadLen
  | OfpbrcBufferEmpty
  | OfpbrcBufferUnknown
  deriving (Enum, Eq, Show)

data OfpBadActionCode =
    OfpbacBadType
  | OfpbacBadLen
  | OfpbacBadVendor
  | OfpbacBadVendorType
  | OfpbacBadOutPort
  | OfpbacBadArgument
  | OfpbacEperm
  | OfpbacTooMany
  | OfpbacBadQueue
  deriving (Enum, Eq, Show)

data OfpFlowModFailedCode =
    OfpfmfcAllTablesFull
  | OfpfmfcOverlap
  | OfpfmfcEperm
  | OfpfmfcBadEmergTimeout
  | OfpfmfcBadCommand
  | OfpfmfcUnsupported
  deriving (Enum, Eq, Show)

data OfpPortModFailedCode =
    OfppmfcBadPort
  | OfppmfcBadHwAddr
  deriving (Enum, Eq, Show)

data OfpQueueOpFailedCode =
    OfpqofcBadPort
  | OfpqofcBadQueue
  | OfpqofcEperm
  deriving (Enum, Eq, Show)

data OfpMatch = OfpMatch
  { mWildcards :: !Word32
  , mInPort    :: !Word16
  , mDlSrc     :: !MAC
  , mDlDst     :: !MAC
  , mDlVlan    :: !Word16
  , mDlVlanPcp :: !Word8
  , mDlType    :: !Word16
  , mNwTos     :: !Word8
  , mNwProto   :: !Word8
  , mNwSrc     :: !Word32
  , mNwDst     :: !Word32
  , mTpSrc     :: !Word16
  , mTpDst     :: !Word16
  } deriving (Eq, Show)


data OfpFlowWildcards =
    OfpfwInPort
  | OfpFwDlVlan
  | OfpFwDlSrc
  | OfpFwDlDst
  | OfpFwDlType
  | OfpFwNwProto
  | OfpFwTpSrc
  | OfpFwTpDst
  | OfpFwNwSrcShift
  | OfpFwNwSrcBits
  | OfpFwNwSrcMask
  | OfpFwNwSrcAll
  | OfpFwNwDstShift
  | OfpFwNwDstBits
  | OfpFwNwDstMask
  | OfpFwNwDstAll
  | OfpFwDlVlanPcp
  | OfpFwNwTos
  | OfpFwAll
  deriving (Eq, Show)

ofpfwTable :: [(OfpFlowWildcards, Int)]
ofpfwTable = [
    (OfpfwInPort,     1 `shiftL` 0)
  , (OfpFwDlVlan,     1 `shiftL` 1)
  , (OfpFwDlSrc,      1 `shiftL` 2)
  , (OfpFwDlDst,      1 `shiftL` 3)
  , (OfpFwDlType,     1 `shiftL` 4)
  , (OfpFwNwProto,    1 `shiftL` 5)
  , (OfpFwTpSrc,      1 `shiftL` 6)
  , (OfpFwTpDst,      1 `shiftL` 7)
  , (OfpFwNwSrcShift, 8)
  , (OfpFwNwSrcBits,  6)
  , (OfpFwNwSrcMask,  ((1 `shiftL` 6) - 1) `shiftL` 8)
  , (OfpFwNwSrcAll,   32 `shiftL` 8)
  , (OfpFwNwDstShift, 14)
  , (OfpFwNwDstBits,  6)
  , (OfpFwNwDstMask,  ((1 `shiftL` 6) - 1) `shiftL` 14)
  , (OfpFwNwDstAll,   32 `shiftL` 14)
  , (OfpFwDlVlanPcp,  1 `shiftL` 20)
  , (OfpFwNwTos,      1 `shiftL` 21)
  , (OfpFwAll,        (1 `shiftL` 22) - 1)
  ]

instance Enum OfpFlowWildcards where
  fromEnum = fromJust . flip lookup ofpfwTable
  toEnum = fromJust . flip lookup (map swap ofpfwTable)


data OfpActionType =
    OfpatOutput
  | OfpatSetVlanVid
  | OfpatSetVlanPcp
  | OfpatStripVlan
  | OfpatSetDlSrc
  | OfpatSetDlDst
  | OfpatSetNwSrc
  | OfpatSetNwDst
  | OfpatSetNwTos
  | OfpatSetTpSrc
  | OfpatSetTpDst
  | OfpatEnqueue
  | OfpatVendor  -- ^ FIXME: fromEnum ==> 0xffff
  deriving (Enum, Eq, Show)

-- | Enumeration of actions
data OfpAction =
    OfpActionOutput {
        aoPort   :: !Word16
      , aoMaxLen :: !Word16
      }
  | OfpActionEnqueue {
        aePort    :: !Word16
      , aeQueueId :: !Word32
      }
  | OfpActionVlanVid { aVlanVid :: !Word16 }
  | OfpActionVlanPcp { aVlanPcp :: !Word8 }
  | OfpActionStripVlan
  | OfpActionSetDlSrc { aDlSrcAddr :: !MAC }
  | OfpActionSetDlDst { aDlDstAddr :: !MAC }
  | OfpActionSetNwSrc { aNwSrcAddr :: !Word32 }
  | OfpActionSetNwDst { aNwDstAddr :: !Word32 }
  | OfpActionSetNwTos { aNwTos     :: !Word8 }
  | OfpActionSetTpSrc { aTpSrcPort :: !Word16 }
  | OfpActionSetTpDst { aTpDstPort :: !Word16 }
  | OfpActionVendor { aVendor :: !Word32 }
  deriving (Eq, Show)


data OfpPhyPort = OfpPhyPort
  { ppPortNo     :: !Word16
  , ppHwAddr     :: !MAC
  , ppName       :: [Word8]
  , ppConfig     :: [OfpPortConfig]
  , ppState      :: [OfpPortState]
  , ppCurr       :: [OfpPortFeatures]
  , ppAdvertised :: [OfpPortFeatures]
  , ppSupported  :: [OfpPortFeatures]
  , ppPeer       :: [OfpPortFeatures]
  } deriving (Eq, Show)


data OfpPortConfig =
    OfppcPortDown
  | OfppcNoStp
  | OfppcNoRecv
  | OfppcNoRecvStp
  | OfppcNoFlood
  | OfppcNoFwd
  | OfppcNoPacketIn
  deriving (Eq, Show)

ofppcTable :: [(OfpPortConfig, Int)]
ofppcTable = [
    (OfppcPortDown,   1 `shiftL` 0)
  , (OfppcNoStp,      1 `shiftL` 1)
  , (OfppcNoRecv,     1 `shiftL` 2)
  , (OfppcNoRecvStp,  1 `shiftL` 3)
  , (OfppcNoFlood,    1 `shiftL` 4)
  , (OfppcNoFwd,      1 `shiftL` 5)
  , (OfppcNoPacketIn, 1 `shiftL` 6)
  ]

instance Enum OfpPortConfig where
  fromEnum = fromJust . flip lookup ofppcTable
  toEnum = fromJust . flip lookup (map swap ofppcTable)


data OfpPortState =
    OfppsLinkDown
  | OfppsStpListen
  | OfppsStpLearn
  | OfppsStpForward
  | OfppsStpBlock
  | OfppsStpMask
  deriving (Eq, Show)

ofppsTable :: [(OfpPortState, Int)]
ofppsTable = [
    (OfppsLinkDown,   1 `shiftL` 0)
  , (OfppsStpListen,  0 `shiftL` 8)
  , (OfppsStpLearn,   1 `shiftL` 8)
  , (OfppsStpForward, 2 `shiftL` 8)
  , (OfppsStpBlock,   3 `shiftL` 8)
  , (OfppsStpMask,    3 `shiftL` 8)
  ]

instance Enum OfpPortState where
  fromEnum = fromJust . flip lookup ofppsTable
  toEnum = fromJust . flip lookup (map swap ofppsTable)


-- FIXME: convert this to above style
type OfpPort = Word16

ofppMax, ofppInPort, ofppTable, ofppNormal, ofppFlood, ofppAll, ofppController, ofppLocal, ofppNone :: OfpPort

-- | Maximum number of physical switch ports.
ofppMax = 0xff00

-- | Fake output "ports".
ofppInPort     = 0xfff8  -- ^ Send the packet out the input port.
ofppTable      = 0xfff9  -- ^ Perform actions in flow table. Packet out only.
ofppNormal     = 0xfffa  -- ^ Process with normal L2/L3 switching.
ofppFlood      = 0xfffb  -- ^ All physical ports except input port and those disabled by STP.
ofppAll        = 0xfffc  -- ^ All physical ports except input port.
ofppController = 0xfffd  -- ^ Send to controller.
ofppLocal      = 0xfffe  -- ^ Local openflow "port".
ofppNone       = 0xffff  -- ^ Not associated with a physical port.


data OfpPortFeatures =
    Ofppf10MbHd
  | Ofppf10MbFd
  | Ofppf100MbHd
  | Ofppf100MbFd
  | Ofppf1GbHd
  | Ofppf1GbFd
  | Ofppf10GbFd
  | OfppfCopper
  | OfppfFiber
  | OfppfAutoneg
  | OfppfPause
  | OfppfPauseAsym
  deriving (Eq, Show)

ofppfTable :: [(OfpPortFeatures, Int)]
ofppfTable = [
    (Ofppf10MbHd,    1 `shiftL` 0)
  , (Ofppf10MbFd,    1 `shiftL` 1)
  , (Ofppf100MbHd,   1 `shiftL` 2)
  , (Ofppf100MbFd,   1 `shiftL` 3)
  , (Ofppf1GbHd,     1 `shiftL` 4)
  , (Ofppf1GbFd,     1 `shiftL` 5)
  , (Ofppf10GbFd,    1 `shiftL` 6)
  , (OfppfCopper,    1 `shiftL` 7)
  , (OfppfFiber,     1 `shiftL` 8)
  , (OfppfAutoneg,   1 `shiftL` 9)
  , (OfppfPause,     1 `shiftL` 10)
  , (OfppfPauseAsym, 1 `shiftL` 11)
  ]
    
instance Enum OfpPortFeatures where
  fromEnum = fromJust . flip lookup ofppfTable
  toEnum = fromJust . flip lookup (map swap ofppfTable)



data OfpPacketQueue = OfpPacketQueue
  { pqQueueId     :: !Word32
  , pqLen        :: !Word16
  , pqProperties :: [OfpQueueProp]
  } deriving (Eq, Show)

data OfpQueueProperties =
    OfpqtNone
  | OfpqtMinRate
  deriving (Enum, Eq, Show)

data OfpQueueProp =
  OfpQueuePropMinRate {
    qpRate :: !Word16
    }
  deriving (Eq, Show)
