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
        datapathId   :: !Word64
      , nBuffers     :: !Word32
      , nTables      :: !Word8
      , capabilities :: [OfpCapabilities]
      , actionTypes  :: [OfpActionType]
      , ports        :: [OfpPhyPort]
      }
  | OfpSwitchConfig {
        flags        :: [OfpConfigFlags]
      , missSendLen  :: !Word16
      }
  | OfpFlowMod {
        match        :: !OfpMatch
      , cookie       :: !Word64
      , command      :: !OfpFlowModCommand
      , idleTimeout  :: !Word16
      , hardTimeout  :: !Word16
      , priority     :: !Word16
      , bufferId     :: !Word32
      , outPort      :: !Word16
      , flags'       :: !OfpFlowModFlags
      , actions      :: [OfpAction]
      }
  | OfpPortMod {
        portNo       :: !Word16
      , hwAddr       :: !MAC
      , config       :: [OfpPortConfig]
      , mask         :: [OfpPortConfig]
      , advertise    :: [OfpPortFeatures]
      }
  | OfpQueueGetConfigRequest {
        port         :: !Word16
      }
  | OfpQueueGetConfigReply {
        port         :: !Word16
      , queues       :: [OfpPacketQueue]
      }
  | OfpStatsRequest {
        ty'          :: !OfpStatsTypes
      , flags''      :: () -- ^ FIXME: undefined??
      , body         :: !OfpRequestStats
      }
  | OfpStatsReply {
        ty''         :: !OfpStatsTypes
      , flags'''     :: () -- ^ FIXME: undefined??
      , body'        :: [OfpReplyStats]
      }
  | OfpPacketOut {
        bufferId     :: !Word32
      , inPort       :: !Word16
      , actionsLen   :: !Word16
      , actions      :: [OfpAction]
      , packet       :: [Word8]
      }
  | OfpPacketIn {
        bufferId'    :: !Word32
      , totalLen     :: !Word16
      , inPort'      :: !Word16
      , reason       :: !OfpPacketInReason
      , packet       :: [Word8]
      }
  | OfpFlowRemoved {
        match'       :: !OfpMatch
      , cookie       :: !Word64
      , priority     :: !Word16
      , reason'      :: !OfpFlowRemovedReason
      , durationSec  :: !Word32
      , durationNsec :: !Word32
      , idleTimeout  :: !Word16
      , packetCount  :: !Word64
      , byteCount    :: !Word64
      }
  | OfpPortStatus {
        reason''     :: !OfpPortReason
      , desc         :: !OfpPhyPort
      }
  | OfpErrorMsg {
        error' :: !OfpError
      , data'  :: [Word8]
      }
  | OfpVendorHeader {
        vendor       :: !Word32
      , data'        :: [Word8]
      }
  | Body [Word8]
  deriving (Eq, Show)

getOfpMessage :: OfpType -> Word16 -> Get OfpMessage
getOfpMessage t l = return $! Body []

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
        match''  :: !OfpMatch
      , tableId  :: !Word8
      , outPort' :: !Word16
      }
  | OfpAggregateStatsRequest {
        match'''' :: !OfpMatch
      , tableId'' :: !Word8
      , outPort'' :: !Word16
      }
  | OfpPortStatsRequest {
        portNo'' :: !Word16
      }
  | OfpQueueStatsRequest {
        portNo''' :: !Word16
      , queueId'' :: !Word32
      }
  deriving (Eq, Show)

data OfpReplyStats =
    OfpDescStats {
        mfrDesc   :: [Word8]
      , hwDesc    :: [Word8]
      , swDesc    :: [Word8]
      , serialNum :: [Word8]
      , dpDesc    :: [Word8]
      }
  | OfpFlowStats {
        length        :: !Word16
      , tableId'      :: !Word8
      , match'''      :: !OfpMatch
      , durationSec'  :: !Word32
      , durationNsec' :: !Word32
      , priority'     :: !Word16
      , idleTimeout'  :: !Word16
      , hardTimeout'  :: !Word16
      , cookie'       :: !Word64
      , packetCount'  :: !Word64
      , byteCount'    :: !Word64
      , actions'      :: [OfpAction]
      }
  | OfpAggregateStatsReply {
        packetCount'' :: !Word64
      , byteCount''   :: !Word64
      , flowCount     :: !Word32
      }
  | OfpTableStats {
        tableId'''   :: !Word8
      , name'        :: [Word8]
      , wildcards'   :: [OfpFlowWildcards]
      , maxEntries   :: !Word32
      , activeCount  :: !Word32
      , lookupCount  :: !Word64
      , matchedCount :: !Word64
      }
  | OfpPortStats {
        portNo'''' :: !Word16
      , rxPackets  :: !Word64
      , txPackets  :: !Word64
      , rxBytes    :: !Word64
      , txBytes    :: !Word64
      , rxDropped  :: !Word64
      , txDropped  :: !Word64
      , rxErrors   :: !Word64
      , txErrors   :: !Word64
      , rxFrameErr :: !Word64
      , rxOverErr  :: !Word64
      , rxCrcErr   :: !Word64
      , collisions :: !Word64
      }
  | OfpQueueStats {
        portNo''''' :: !Word16
      , queueId'''  :: !Word32
      , txBytes'    :: !Word64
      , txPackets'  :: !Word64
      , txErrors'   :: !Word64
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
  { wildcards :: !Word32
  , inPort''  :: !Word16
  , dlSrc     :: !MAC
  , dlDst     :: !MAC
  , dlVlan    :: !Word16
  , dlVlanPcp :: !Word8
  , dlType    :: !Word16
  , nwTos     :: !Word8
  , nwProto   :: !Word8
  , nwSrc     :: !Word32
  , nwDst     :: !Word32
  , tpSrc     :: !Word16
  , tpDst     :: !Word16
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
        port'  :: !Word16
      , maxLen :: !Word16
      }
  | OfpActionEnqueue {
        port''   :: !Word16
      , queueId' :: !Word32
      }
  | OfpActionVlanVid { vlanVid :: !Word16 }
  | OfpActionVlanPcp { vlanPcp :: !Word8 }
  | OfpActionStripVlan
  | OfpActionSetDlSrc { dlAddr :: !MAC }
  | OfpActionSetDlDst { dlAddr' :: !MAC }
  | OfpActionSetNwSrc { nwAddr :: !Word32 }
  | OfpActionSetNwDst { nwAddr' :: !Word32 }
  | OfpActionSetNwTos { nwTos' :: !Word8 }
  | OfpActionSetTpSrc { tpPort :: !Word16 }
  | OfpActionSetTpDst { tpPort' :: !Word16 }
  | OfpActionVendor { vendor' :: !Word32 }
  deriving (Eq, Show)


data OfpPhyPort = OfpPhyPort
  { portNo'    :: !Word16
  , hwAddr'    :: !MAC
  , name       :: [Word8]
  , config'    :: [OfpPortConfig]
  , state      :: [OfpPortState]
  , curr       :: [OfpPortFeatures]
  , advertised :: [OfpPortFeatures]
  , supported  :: [OfpPortFeatures]
  , peer       :: [OfpPortFeatures]
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
  { queueId     :: !Word32
  , len'        :: !Word16
  , qProperties :: [OfpQueueProp]
  } deriving (Eq, Show)

data OfpQueueProperties =
    OfpqtNone
  | OfpqtMinRate
  deriving (Enum, Eq, Show)

data OfpQueueProp =
  OfpQueuePropMinRate {
    rate :: !Word16
    }
  deriving (Eq, Show)
