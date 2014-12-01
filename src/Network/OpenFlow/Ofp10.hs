{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Network.OpenFlow.Ofp10
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
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

    -- * Re-exports
    -- $reexports
  , module Network.OpenFlow.Ofp10.Flow
  , module Network.OpenFlow.Ofp10.Port
  , module Network.OpenFlow.Ofp10.Queue
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
import Network.OpenFlow.Ofp10.Flow
import Network.OpenFlow.Ofp10.Port
import Network.OpenFlow.Ofp10.Queue

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
