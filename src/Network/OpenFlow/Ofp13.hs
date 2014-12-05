{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Network.OpenFlow.Ofp13
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  experimental
-- Portability :  unknown
--
-- OpenFlow Switch Specification Version 1.3.4
--------------------------------------------------------------------------------
module Network.OpenFlow.Ofp13
  (
  -- * Messages
    OfpFrame(..)
  , OfpHeader(..)
  , OfpType(..)
  , OfpMessage(..)

  -- * Port Structures
  -- | Ports are numbered starting from 1.
  , OfpPortNo
  , ofppMax
  , ofppInPort
  , ofppTable
  , ofppNormal
  , ofppFlood
  , ofppAll
  , ofppController
  , ofppLocal
  , ofppAny

  ) where

import Data.Bits
import GHC.Word
import Network.MAC

data OfpFrame = OfpFrame
  { header  :: !OfpHeader
  , message :: !OfpMessage
  } deriving (Show,Eq)

-- | 7.1.1 OpenFlow Header

ofp13Version :: Int
ofp13Version = 0x04

data OfpHeader = OfpHeader
  { version :: !Word8   -- ^ OpenFlow protocol version.
  , ty      :: !OfpType
  , len     :: !Word16
  , xid     :: !Word32  -- ^ Transaction id associated with this packet. Replies use the same id as was in the request to facilitate pairing.
  } deriving (Show,Eq)

data OfpType =
  -- | Immutable messages.
    OfptHello
  | OfptError
  | OfptEchoRequest
  | OfptEchoReply
  | OfptExperimenter
  -- | Switch configuration messages.
  | OfptFeaturesRequest
  | OfptFeaturesReply
  | OfptSetConfig
  -- | Asynchronous messages.
  | OfptPacketIn
  | OfptFlowRemoved
  | OfptPortStatus
  -- | Controller command messages.
  | OfptPacketOut
  | OfptFlowMod
  | OfptGroupMod
  | OfptPortMod
  | OfptTableMod
  -- | Multipart messages.
  | OfptMultipartRequest
  | OfptMultipartReply
  -- | Barrier messages.
  | OfptBarrierRequest
  | OfptBarrierReply
  -- | Queue configuration messages.
  | OfptQueueGetConfigRequest
  | OfptQueueGetConfigReply
  -- | Controller role change request messages.
  | OfptRoleRequest
  | OfptRoleReply
  -- | Asynchronous message configuration.
  | OfptGetAsyncRequest
  | OfptGetAsyncReply
  | OfptSetAsync
  -- | Meters and rate limiters configuration messages.
  | OfptMeterMod
  deriving (Show,Eq)

data OfpMessage =
    OfpSwitchFeatures {
      sfDataPathId   :: !Word64 -- ^ Datapath unique ID. The lower 48-bits are for a MAC address, while the upper 16-bits are implementer-defined.
    , sfNBuffers     :: !Word32 -- ^ Max packets buffered at once.
    , sfNTables      :: !Word8  -- ^ Number of tables suppoerted by datapath.
    , sfAuxiliaryId  :: !Word8
    , sfCapabilities :: () -- [OfpCapabilities]
    , sfReserved     :: !Word32
    }
  | OfpSwitchConfig {
      scFlags        :: () --[Ofpc*]
    , scMissSendLen  :: !Word16
    }
  | OfpTableMod {
      tmTableId      :: !Word8
    , tmConfig       :: () -- [Ofptc*]
    }
  | OfpFlowMod {
      fmCookie       :: !Word64
    , fmCookieMask   :: !Word64
    , fmTableId      :: !Word8
    , fmCommand      :: () -- Ofpfc*
    , fmIdleTimeout  :: !Word16
    , fmHardTimeout  :: !Word16
    , fmPriority     :: !Word16
    , fmBufferId     :: !Word32
    , fmOutPort      :: !Word32
    , fmOutGroup     :: !Word32
    , fmFlags        :: () -- [Ofpff*]
    , fmMatch        :: () -- [OfpMatch*]
    , fmInstructions :: () -- [OfpInstruction*]
    }
  | OfpGroupMod {
      gmCommand :: () -- Ofpgc*
    , gmType    :: () -- Ofpgt*
    , gmGroupId :: !Word32
    , gmBuckets :: () -- [OfpBucket*]
    }
  | OfpPortMod {
      pmPortNo    :: !Word32
    , pmHwAddr    :: !MAC
    , pmConfig    :: () -- [Ofppc*]
    , pmMask      :: () -- [Ofppc*]
    , pmAdvertise :: () -- [Ofppf*]
    }
  | OfpMeterMod {
      mmCommand :: () -- Ofpmc*
    , mmFlags   :: () -- [Ofpmf*]
    , mmMeterId :: !Word32
    , mmBands   :: () -- [OfpMeterBand*]
    }
  | OfpMultipartRequest {
      mpReqType  :: () -- Ofpmp*
    , mpReqFlags :: () -- [OfpmpfReq*]
    , mpReqBody  :: [Word8]
    }
  | OfpMultipartReply {
      mpRepType  :: () -- Ofpmp*
    , mpRepFlags :: () -- [OfpmpfRep*]
    , mpRepBody  :: [Word8]
    }
  | OfpQueueGetConfigRequest {
      qReqPort   :: !Word32
    }
  | OfpQueueGetConfigReply {
      qRepPort   :: !Word32
    , qRepQueues :: () -- [OfpPacketQueue*]
    }
  | OfpPacketOut {
      poBufferId   :: !Word32
    , poInPort     :: !Word32
    , poActionsLen :: !Word16
    , poActions    :: () -- [OfpAction*]
    , poData       :: [Word8]
    }
  | OfpRoleRequest {
      rReqRole         :: () -- Ofpcr*
    , rReqGenerationId :: !Word64
    }
  | OfpAsyncConfig {
      acPacketInMask    :: () -- [Ofpr*]
    , acPortStatusMask  :: () -- [Ofppr*]
    , acFlowRemovedMask :: () -- [Ofprr*]
    }
  | OfpPacketIn {
      piBufferId :: !Word32
    , piTotalLen :: !Word16
    , piReason   :: () -- Ofpr*
    , piTableId  :: !Word8
    , piCookie   :: !Word64
    , piMatch    :: () -- [OfpMatch*]
    , piData     :: [Word8]
    }
  | OfpFlowRemoved {
      frCookie       :: !Word64
    , frPriority     :: !Word16
    , frReason       :: () -- Ofprr*
    , frTableId      :: !Word8
    , frDurationSec  :: !Word32
    , frDurationNSec :: !Word32
    , frIdleTimeout  :: !Word16
    , frHardTimeout  :: !Word16
    , frPacketCount  :: !Word64
    , frByteCount    :: !Word64
    , frMatch        :: () -- OfpMatch*
    }
  | OfpPortStatus {
      psReason :: () -- Ofppr*
    , psDesc   :: () -- OfpPort*
    }
  | OfpErrorMsg {
      eType :: !Word16
    , eCode :: !Word16
    , eData :: [Word8]
    }
  | OfpErrorExperimenterMsg {
      eeType         :: !Word16
    , eeExpType      :: !Word16
    , eeExperimenter :: !Word32
    , eeData         :: [Word8]
    }
  | OfpHello {
      hElements :: () -- [OfpHelloElem*]
    }
  | OfpExperimenterHeader {
      ehExperimenter :: !Word32
    , ehExpType      :: !Word32
    , ehData         :: [Word8]
    }
  deriving (Show, Eq)


type OfpPortNo = Word32

-- | Maxiumum number of physical and logical switch ports.
ofppMax :: OfpPortNo
ofppMax = 0xffffff00
-- | Send the packet out the input port. This reserved port must be explicitly used in order to send back out of the input port.
ofppInPort :: OfpPortNo
ofppInPort = 0xfffffff8
-- | Submit the packet to the first flow table NB: This destination port can only be used in packet-out messages.
ofppTable :: OfpPortNo
ofppTable = 0xfffffff9
-- | Forward using non-OpenFlow pipeline.
ofppNormal :: OfpPortNo
ofppNormal = 0xfffffffa
-- | Flood using non-OpenFlow pipeline.
ofppFlood :: OfpPortNo
ofppFlood = 0xfffffffb
-- | All standard ports except input port.
ofppAll :: OfpPortNo
ofppAll = 0xfffffffc
-- | Send to controller.
ofppController :: OfpPortNo
ofppController = 0xfffffffd
-- | Local openflow "port".
ofppLocal :: OfpPortNo
ofppLocal = 0xfffffffe
-- | Special value used in some requets when no port is specified (i.e. wildcarded).
ofppAny :: OfpPortNo
ofppAny = 0xffffffff

data OfpPort = OfpPort
  { pPortNo     :: !Word32
  , pHwAddr     :: !MAC
  , pName       :: [Word8]
  , pConfig     :: [OfpPortConfig]
  , pState      :: [OfpPortState]
  , pCurr       :: [OfpPortFeatures]
  , pAdvertised :: [OfpPortFeatures]
  , pSupported  :: [OfpPortFeatures]
  , pPeer       :: [OfpPortFeatures]
  , pCurrSpeed  :: !Word32
  , pMaxSpeed   :: !Word32
  } deriving (Show, Eq)


data OfpPortConfig =
    OfppcPortDown
  | OfppcNoRecv
  | OfppcNoFwd
  | OfppcNoPacketIn
    deriving (Show, Eq)

data OfpPortState =
    OfppsLinkDown
  | OfppsBlocked
  | OfppsLive
  deriving (Show, Eq)

data OfpPortFeatures =
    Ofppf10MbHD
  | Ofppf10MbFD
  | Ofppf100MbHD
  | Ofppf100MbFD
  | Ofppf1GbHD
  | Ofppf1GbFD
  | Ofppf10GbFD
  | Ofppf40GbFD
  | Ofppf100GbFD
  | Ofppf1TbFD
  | OfppfOther
  | OfppfCopper
  | OfppfFiber
  | OfppfAutoneg
  | OfppfPause
  | OfppfPauseAsym
  deriving (Show, Eq)

-- | 7.2.2 Queue Structures

data OfpPacketQueue = OfpPacketQueue
  { pqQueueId :: !Word32
  , pqPort    :: !Word32
  , pqLen     :: !Word16
  , pqProperties :: [OfpQueueProp]
  } deriving (Show, Eq)

data OfpQueueProperties =
    OfpqtMinRate
  | OfpqtMaxRate
  | OfpqtExperimenter
  deriving (Enum, Eq, Show)

data OfpQueueProp =
    OfpQueuePropMinRate {
        qpMinRate      :: !Word16
      }
  | OfpQueuePropMaxRate {
        qpMaxRate      :: !Word16
      }
  | OfpQueuePropExperimenter {
        qpExperimenter :: !Word32
      , qpData         :: [Word8]
      }
  deriving (Eq, Show)

-- | 7.2.3 Flow Match Structures

data OfpMatch = OfpMatch
  { mTy        :: !OfpMatchType
  , mLen       :: !Word16
  , mOxmFields :: () -- [OxmTlv]
  }

data OfpMatchType =
    OfpmtStandard
  | OfpmtOxm
  deriving (Enum, Eq, Show)


data OfpOxmClass =
    OfpxmcNxm0
  | OfpxmcNxm1
  | OfpxmcOpenFlowBasic
  | OfpxmcExperimenter
  deriving (Eq, Show)

data OxmOfpMatchFields =
    OfpxmtOfpInPort
  | OfpxmtOfpInPhyPort
  | OfpxmtOfpMetaData
  | OfpxmtOfpEthDst
  | OfpxmtOfpEthSrc
  | OfpxmtOfpEthType
  | OfpxmtOfpVlanVid
  | OfpxmtOfpVlanPcp
  | OfpxmtOfpIpDscp
  | OfpxmtOfpIpEcn
  | OfpxmtOfpIpProto
  | OfpxmtOfpIpv4Src
  | OfpxmtOfpIpv4Dst
  | OfpxmtOfpTcpSrc
  | OfpxmtOfpTcpDst
  | OfpxmtOfpUdpSrc
  | OfpxmtOfpUdpDst
  | OfpxmtOfpSctpSrc
  | OfpxmtOfpSctpDst
  | OfpxmtOfpIcmpv4Type
  | OfpxmtOfpIcmpv4Code
  | OfpxmtOfpArpOp
  | OfpxmtOfpArpSpa
  | OfpxmtOfpArpTpa
  | OfpxmtOfpArpSha
  | OfpxmtOfpArpTha
  | OfpxmtOfpIpv6Src
  | OfpxmtOfpIpv6Dst
  | OfpxmtOfpIpv6Flabel
  | OfpxmtOfpIcmpv6Type
  | OfpxmtOfpIcmpv6Code
  | OfpxmtOfpIpv6NdTarget
  | OfpxmtOfpIpv6NdSll
  | OfpxmtOfpIpv6NdTll
  | OfpxmtOfpMplsLabel
  | OfpxmtOfpMplsTc
  | OfpxmtOfpMplsBos
  | OfpxmtOfpPbbIsid
  | OfpxmtOfpTunnelId
  | OfpxmtOfpIpv6Exthdr
  deriving (Enum, Eq, Show)

data OfpVlanId =
    OfpvidPresent
  | OfpvidNone
  deriving (Eq, Show)

data OfpIpv6exthdrFlags =
    OfpiehNoNext
  | OfpiehEsp
  | OfpiehAuth
  | OfpiehDest
  | OfpiehFrag
  | OfpiehRouter
  | OfpiehHop
  | OfpiehUnrep
  | OfpiehUnseq
  deriving (Eq, Show)

-- | 7.2.4 Flow Instruction Structures

data OfpInstructionType =
    OfpitGotoTable
  | OfpitWriteMetadata
  | OfpitWriteActions
  | OfpitApplyActions
  | OfpitClearActions
  | OfpitMeter
  | OfpitExperimenter
  deriving (Enum, Eq, Show)

-- data OfpInstruction =
--   OfpInstructionGotoTable {
--       igtLen     :: !Word16
--     , igtTableId :: !Word8
--     }
--   | OfpInstructionWriteMetadata {
--       iwmLen          :: !Word16
--     , iwmMetadata     :: !Word64
--     , iwmMetadataMask :: !Word64
--     }
--   | OfpInstructionActions {
--     , iaTy        :: 
--                           }

-- | 7.2.5 Action Structures

-- | 7.3 Controller-to-Switch Messages

-- | 7.3.1 Handshake
-- | 7.3.2 Switch Configuration
-- | 7.3.3 Flow Table Configuration
-- | 7.3.4 Modify State Messages
-- | 7.3.5 Multipart Messages
-- | 7.3.6 Queue Configuration Messages
-- | 7.3.7 Packet-Out Message
-- | 7.3.8 Barrier Message
-- | 7.3.9 Role Request Message
-- | 7.3.10 Set Asynchronous Configuration Message

-- | 7.4 Asynchronous Messages

-- | 7.4.1 Packet-In Message
-- | 7.4.2 Flow Removed Message
-- | 7.4.3 Port Status Message
-- | 7.4.4 Error Message

-- | 7.5 Symmetric Messages

-- | 7.5.1 Hello
-- | 7.5.2 Echo Request
-- | 7.5.3 Echo Reply
-- | 7.5.4 Experimenter

