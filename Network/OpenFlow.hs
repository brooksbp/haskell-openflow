module Network.OpenFlow (
    OfpFrame(..)
  , OfpHeader(..)
  , OfpMsg(..)
  , OfpError(..)
  , OfpHelloFailedCode(..)
  , OfpBadRequestCode(..)
  , OfpBadActionCode(..)
  , OfpFlowModFailedCode(..)
  , OfpPortModFailedCode(..)
  , OfpQueueOpFailedCode(..)
  , OfpSwitchFeatures(..)
  , OfpPhyPort(..)
  , OfpCapabilities(..)
  , OfpActionType(..)
  , OfpPortConfig(..)
  , OfpPortState(..)
  , OfpPortFeatures(..)
  , OfpSwitchConfig(..)
  , OfpConfigFlags(..)
  , OfpFlowMod(..)
  , OfpMatch(..)
  , OfpFlowModCommand(..)
  , OfpFlowModFlags(..)
  , OfpAction(..)
  , OfpFlowWildcards(..)
  , OfpPacketIn(..)
  , OfpPacketInReason(..)
  , OfpPacketOut(..)
  , readOfpFrame
  ) where

import GHC.Word
import Control.Monad
import Data.Functor
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Network.Info


data OfpFrame =
  -- 5.1 Each OpenFlow message begins with the OpenFlow header
  OfpFrame { hdr :: OfpHeader
           , msg :: OfpMsg
           } deriving (Show)

instance Serialize OfpFrame where
  put (OfpFrame (OfpHeader version _ _ xid) msg) = do
    putWord8 version
    putWord8 $ msgType msg
    let bs = runPut $ putOfpMsg msg
    let len = ofpHdrLen + (BS.length bs)
    put ((fromIntegral len) :: Word16)
    putWord32be xid
    putByteString bs
  get = do
    hdr <- get :: Get OfpHeader
    msg <- getOfpMsg hdr
    return $ OfpFrame hdr msg

data OfpHeader =
  OfpHeader { version :: Word8
            , ty      :: Word8
            , len     :: Word16
            , xid     :: Word32
            } deriving (Show)

ofpHdrLen = 8

instance Serialize OfpHeader where
  put (OfpHeader version ty len xid) = do
    putWord8 version
    putWord8 ty
    putWord16be len
    putWord32be xid
  get = do
    version <- liftM fromIntegral getWord8
    ty      <- liftM fromIntegral getWord8
    len     <- liftM fromIntegral getWord16be
    xid     <- liftM fromIntegral getWord32be
    return $ OfpHeader version ty len xid

data OfpMsg = OfptHello [Word8]
              -- 5.5.1 Implementations must be prepared to receive a
              -- Hello message that includes a body, ignoring its contents.
            | OfptError OfpError
            | OfptEchoRequest [Word8]
              -- 5.5.2 Echo Request may or may not include a data field.
            | OfptEchoReply [Word8]
              -- 5.5.3 An Echo Reply consist of the unmodified data field of an
              -- Echo Request message.
            | OfptVendor
            | OfptFeaturesRequest
            | OfptFeaturesReply OfpSwitchFeatures
            | OfptGetConfigRequest
            | OfptGetConfigReply OfpSwitchConfig
            | OfptSetConfig OfpSwitchConfig
            | OfptPacketIn OfpPacketIn
            | OfptPacketOut OfpPacketOut
            | OfptFlowMod OfpFlowMod
            | OfptBarrierRequest
            | OfptBarrierReply
            deriving (Show)

-- | Immutable messages
msgType (OfptHello _)            = 0
msgType (OfptError _)            = 1
msgType (OfptEchoRequest _)      = 2
msgType (OfptEchoReply _)        = 3
msgType (OfptVendor)             = 4
-- | Switch configuration messages
msgType (OfptFeaturesRequest)    = 5
msgType (OfptFeaturesReply _)    = 6
msgType (OfptGetConfigRequest)   = 7
msgType (OfptGetConfigReply _)   = 8
msgType (OfptSetConfig _)        = 9
-- | Asynchronous messages
msgType (OfptPacketIn _)         = 10
--msgType (OfptFlowRemoved)        = 11
--msgType (OfptPortStatus)         = 12
-- | Controller command messages
msgType (OfptPacketOut _)        = 13
msgType (OfptFlowMod _)          = 14
--msgType (OfptPortMod)            = 15
-- | Statistics messages
--msgType (OfptStatsRequest)       = 16
--msgType (OfptStatsReply)         = 17
-- | Barrier messages
msgType (OfptBarrierRequest)     = 18
msgType (OfptBarrierReply)       = 19
-- | Queue configuration messages
--msgType (OfptQueueGetConfigRequest)   = 20
--msgType (OfptQueueGetConfigReply)     = 21

data OfpError = OfpetHelloFailed OfpHelloFailedCode String
              | OfpetBadRequest OfpBadRequestCode
              | OfpetBadAction OfpBadActionCode
              | OfpetFlowModFailed OfpFlowModFailedCode
              | OfpetPortModFailed OfpPortModFailedCode
              | OfpetQueueOpFailed OfpQueueOpFailedCode
              deriving (Show)

data OfpHelloFailedCode = OfphfcIncompatible
                        | OfphfcEperm
                        deriving (Show, Enum)

data OfpBadRequestCode = OfpbrcBadVersion
                       | OfpbrcBadType
                       | OfpbrcBadStat
                       | OfpbrcBadVendor
                       | OfpbrcBadSubtype
                       | OfpbrcEperm
                       | OfpbrcBadLen
                       | OfpbrcBufferEmpty
                       | OfpbrcBufferUnknown
                       deriving (Show, Enum)

data OfpBadActionCode = OfpbacBadType
                      | OfpbacBadLen
                      | OfpbacBadVendor
                      | OfpbacBadVendorType
                      | OfpbacBadOutPort
                      | OfpbacBadArgument
                      | OfpbacEperm
                      | OfpbacTooMany
                      | OfpbacBadQueue
                      deriving (Show, Enum)

data OfpFlowModFailedCode = OfpfmfcAllTablesFull
                          | OfpfmfcOverlap
                          | OfpfmfcEperm
                          | OfpfmfcBadEmergTimeout
                          | OfpfmfcBadCommand
                          | OfpfmfcUnsupported
                          deriving (Show, Enum)

data OfpPortModFailedCode = OfppmfcBadPort
                          | OfppmfcBadHwAddr
                          deriving (Show, Enum)

data OfpQueueOpFailedCode = OfpqofcBadPort
                          | OfpqofcBadQueue
                          | OfpqofcEperm
                          deriving (Show, Enum)

data OfpSwitchFeatures =
  OfpSwitchFeatures { dataPathId   :: Word64
                    , nBuffers     :: Word32
                    , nTables      :: Word8
                    , capabilities :: [OfpCapabilities]
                    , actions      :: [OfpActionType]
                    , ports        :: [OfpPhyPort]
                    } deriving (Show)

data OfpCapabilities = OfpcFlowStats
                     | OfpcTableStats
                     | OfpcPortStats
                     | OfpcStp
                     | OfpcReserved
                     | OfpcIpReasm
                     | OfpcQueueStats
                     | OfpcArpMatchIp
                     deriving (Show, Enum)

data OfpActionType = OfpatOutput
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
                   | OfpatVendor
                   deriving (Show, Enum)

data OfpPhyPort =
  OfpPhyPort { portNo :: Word16
             , hwAddr :: MAC
             , name   :: String
             , config :: [OfpPortConfig]
             , state  :: [OfpPortState]
             , curr       :: [OfpPortFeatures]
             , advertised :: [OfpPortFeatures]
             , supported  :: [OfpPortFeatures]
             , peer       :: [OfpPortFeatures]
             } deriving (Show)

instance Serialize MAC where
  put (MAC w1 w2 w3 w4 w5 w6) = do
    putWord8 w1
    putWord8 w2
    putWord8 w3
    putWord8 w4
    putWord8 w5
    putWord8 w6
  get = do
    w1 <- liftM fromIntegral getWord8
    w2 <- liftM fromIntegral getWord8
    w3 <- liftM fromIntegral getWord8
    w4 <- liftM fromIntegral getWord8
    w5 <- liftM fromIntegral getWord8
    w6 <- liftM fromIntegral getWord8
    return $ MAC w1 w2 w3 w4 w5 w6

ofpMaxPortNameLen = 16

prependSpace xs 0 = xs
prependSpace xs n = prependSpace (" " ++ xs) (n-1)

instance Serialize OfpPhyPort where
  put (OfpPhyPort portNo hwAddr name config state curr advertised supported peer) = do
    putWord16be portNo
    put hwAddr
    putByteString $ C.pack $ prependSpace name (ofpMaxPortNameLen - (length name))
    putWord32be $ enumToBitInst config
    putWord32be $ enumToBitInst state
    putWord32be $ enumToBitInst curr
    putWord32be $ enumToBitInst advertised
    putWord32be $ enumToBitInst supported
    putWord32be $ enumToBitInst peer
  get = do
    portNo <- liftM fromIntegral getWord16be
    hwAddr <- get :: Get MAC
    nameBS <- getByteString ofpMaxPortNameLen
    let name = C.unpack nameBS
    configBits     <- getWord32be
    stateBits      <- getWord32be
    currBits       <- getWord32be
    advertisedBits <- getWord32be
    supportedBits  <- getWord32be
    peerBits       <- getWord32be
    let config     = word32ToEnum configBits
    let state      = word32ToEnum stateBits
    let curr       = word32ToEnum currBits
    let advertised = word32ToEnum advertisedBits
    let supported  = word32ToEnum supportedBits
    let peer       = word32ToEnum peerBits
    return $ OfpPhyPort portNo hwAddr name config state curr advertised supported peer

data OfpPortConfig = OfppcPortDown
                   | OfppcNoStp
                   | OfppcNoRecv
                   | OfppcNoRecvStp
                   | OfppcNoFlood
                   | OfppcNoFwd
                   | OfppcNoPacketIn
                   deriving (Show, Enum)

data OfpPortState = OfppsLinkDown
                  | OfppsStpListen
                  | OfppsStpLearn
                  | OfppsStpForward
                  | OfppsStpBlock
                  | OfppsStpMask
                  deriving (Show, Enum)

data OfpPortFeatures = Ofppf10MbHd
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
                     deriving (Show, Enum)

data OfpSwitchConfig =
  OfpSwitchConfig { flags       :: [OfpConfigFlags]
                  , missSendLen :: Word16
                  } deriving (Show)

instance Serialize OfpSwitchConfig where
  put (OfpSwitchConfig flags missSendLen) = do
    putWord16be $ enumToBitInst flags
    putWord16be missSendLen
  get = do
    flagsBits <- getWord16be
    let flags = word16ToEnum flagsBits
    missSendLen <- getWord16be
    return $ OfpSwitchConfig flags missSendLen  

data OfpConfigFlags = OfpcFragNormal
                    | OfpcFragDrop
                    | OfpcFragReasm
                    | OfpcFragMask
                    deriving (Show, Enum)

data OfpFlowMod =
  OfpFlowMod { match       :: OfpMatch
             , cookie      :: Word64
             , command     :: OfpFlowModCommand
             , idleTimeout :: Word16
             , hardTimeout :: Word16
             , priority    :: Word16
             , bufferId    :: Word32
             , outPort     :: Word16
             , fmFlags     :: OfpFlowModFlags
             , fmActions   :: [OfpAction]
             } deriving (Show)               

instance Serialize OfpFlowMod where
  put (OfpFlowMod match cookie command idleTimeout hardTimeout priority bufferId outPort flags actions) = do
    put match
    putWord64be cookie
    putWord16be $ fromIntegral $ fromEnum command
    putWord16be idleTimeout
    putWord16be hardTimeout
    putWord16be priority
    putWord32be bufferId
    putWord16be outPort
    putWord16be $ fromIntegral $ fromEnum flags
    mapM_ put actions
  get = do
    match       <- get :: Get OfpMatch
    cookie      <- getWord64be
    command'    <- liftM fromIntegral getWord16be
    let command = toEnum command' :: OfpFlowModCommand
    idleTimeout <- getWord16be
    hardTimeout <- getWord16be
    priority    <- getWord16be
    bufferId    <- getWord32be
    outPort     <- getWord16be
    flags'      <- liftM fromIntegral getWord16be
    let flags = toEnum flags' :: OfpFlowModFlags
    rem <- remaining
    actionsBS   <- getByteString rem
    let actions = readMany actionsBS
    return $ OfpFlowMod match cookie command idleTimeout hardTimeout priority bufferId outPort flags actions

data OfpAction = OfpOutput     Word16 Word16
               | OfpSetVlanVid Word16
               | OfpSetVlanPcp Word8
               | OfpStripVlan
               | OfpSetDlSrc   MAC
               | OfpSetDlDst   MAC
               | OfpSetNwSrc   Word32
               | OfpSetNwDst   Word32
               | OfpSetNwTos   Word8
               | OfpSetTpSrc   Word16
               | OfpSetTpDst   Word16
               | OfpEnqueue    Word16 Word32
               | OfpVendor
               deriving (Show)

instance Serialize OfpAction where
  put (OfpOutput port maxLen) = do
    putWord16be 0
    putWord16be 8
    putWord16be port
    putWord16be maxLen
  put (OfpSetVlanVid vlanVid) = do
    putWord16be 1
    putWord16be 8
    putWord16be vlanVid
    putWord16be 0 -- pad
  put (OfpSetVlanPcp vlanPcp) = do
    putWord16be 2
    putWord16be 8
    putWord8 vlanPcp
    putWord8 0 -- pad
    putWord8 0 -- pad
    putWord8 0 -- pad
  put (OfpStripVlan) = do
    putWord16be 3
    putWord16be 8
    putWord32be 0 -- pad
  put (OfpSetDlSrc mac) = do
    putWord16be 4
    putWord16be 16
    put mac
    putWord16be 0 -- pad
    putWord16be 0 -- pad
    putWord16be 0 -- pad
  put (OfpSetDlDst mac) = do
    putWord16be 5
    putWord16be 16
    put mac
    putWord16be 0 -- pad
    putWord16be 0 -- pad
    putWord16be 0 -- pad
  put (OfpSetNwSrc nwAddr) = do
    putWord16be 6
    putWord16be 8
    putWord32be nwAddr
  put (OfpSetNwDst nwAddr) = do
    putWord16be 7
    putWord16be 8
    putWord32be nwAddr
  put (OfpSetNwTos nwTos) = do
    putWord16be 8
    putWord16be 8
    putWord8 nwTos
    putWord8 0 -- pad
    putWord8 0 -- pad
    putWord8 0 -- pad
  put (OfpSetTpSrc tpPort) = do
    putWord16be 9
    putWord16be 8
    putWord16be tpPort
    putWord16be 0 -- pad
  put (OfpSetTpDst tpPort) = do
    putWord16be 10
    putWord16be 8
    putWord16be tpPort
    putWord16be 0 -- pad
  put (OfpEnqueue port queueId) = do
    putWord16be 11
    putWord16be 16
    putWord16be port
    putWord16be 0 -- pad
    putWord32be 0 -- pad
    putWord32be queueId
  put (OfpVendor) = do
    putWord16be 0xffff
    putWord16be 8
    putWord32be 0 -- pad
  get = do
    ty  <- getWord16be
    len <- getWord16be
    case ty of
      0 -> do
        port   <- getWord16be
        maxLen <- getWord16be
        return $ OfpOutput port maxLen
      1 -> do
        vlanVid <- getWord16be
        pad1    <- getWord16be
        return $ OfpSetVlanVid vlanVid
      2 -> do
        vlanPcp <- getWord8
        pad1 <- getWord8
        pad2 <- getWord8
        pad3 <- getWord8
        return $ OfpSetVlanPcp vlanPcp
      3 -> do
        pad1 <- getWord32be
        return $ OfpStripVlan
      4 -> do
        mac <- get :: Get MAC
        pad1 <- getWord16be
        pad2 <- getWord16be
        pad3 <- getWord16be
        return $ OfpSetDlSrc mac
      5 -> do
        mac <- get :: Get MAC
        pad1 <- getWord16be
        pad2 <- getWord16be
        pad3 <- getWord16be
        return $ OfpSetDlDst mac
      6 -> do
        nwAddr <- getWord32be
        return $ OfpSetNwSrc nwAddr
      7 -> do
        nwAddr <- getWord32be
        return $ OfpSetNwDst nwAddr
      8 -> do
        nwTos <- getWord8
        pad1 <- getWord8
        pad2 <- getWord8
        pad2 <- getWord8
        return $ OfpSetNwTos nwTos
      9 -> do
        tpPort <- getWord16be
        pad1 <- getWord16be
        return $ OfpSetTpSrc tpPort
      10 -> do
        tpPort <- getWord16be
        pad1 <- getWord16be
        return $ OfpSetTpDst tpPort
      11 -> do
        port <- getWord16be
        pad1 <- getWord16be
        pad2 <- getWord32be
        queueId <- getWord32be
        return $ OfpEnqueue port queueId
      0xffff -> return OfpVendor


data OfpMatch =
  OfpMatch { wildcards :: [OfpFlowWildcards]
           , inPort    :: Word16
           , dlSrc     :: MAC
           , dlDst     :: MAC
           , dlVlan    :: Word16
           , dlVlanPcp :: Word8
           , dlType    :: Word16
           , nwTos     :: Word8
           , nwProto   :: Word8
           , nwSrc     :: Word32
           , nwDst     :: Word32
           , tpSrc     :: Word16
           , tpDst     :: Word16
           } deriving (Show)

instance Serialize OfpMatch where
  put (OfpMatch wildcards inPort dlSrc dlDst dlVlan dlVlanPcp dlType nwTos nwProto nwSrc nwDst tpSrc tpDst) = do
    putWord32be $ enumToBitInst wildcards
    putWord16be inPort
    put dlSrc
    put dlDst
    putWord16be dlVlan
    putWord8    dlVlanPcp
    putWord8 0
    putWord16be dlType
    putWord8    nwTos
    putWord8    nwProto
    putWord8 0
    putWord8 0
    putWord32be nwSrc
    putWord32be nwDst
    putWord16be tpSrc
    putWord16be tpDst
  get = do
    wildcardBits <- getWord32be
    let wildcards = word32ToEnum wildcardBits
    inPort       <- getWord16be
    dlSrc <- get :: Get MAC
    dlDst <- get :: Get MAC
    dlVlan    <- getWord16be
    dlVlanPcp <- getWord8
    pad1      <- getWord8
    dlType    <- getWord16be
    nwTos     <- getWord8
    nwProto   <- getWord8
    pad2      <- getWord8
    pad3      <- getWord8
    nwSrc     <- getWord32be
    nwDst     <- getWord32be
    tpSrc     <- getWord16be
    tpDst     <- getWord16be
    return $ OfpMatch wildcards inPort dlSrc dlDst dlVlan dlVlanPcp dlType nwTos nwProto nwSrc nwDst tpSrc tpDst

data OfpFlowWildcards = OfpfwInPort
                      | OfpfwDlVlan
                      | OfpfwDlSrc
                      | OfpfwDlDst
                      | OfpfwDlType
                      | OfpfwNwProto
                      | OfpfwTpSrc
                      | OfpfwTpDst
                      | OfpfwNwSrcShift
                      | OfpfwNwSrcBits
                      | OfpfwNwSrcMask
                      | OfpfwNwSrcAll
                      | OfpfwDlVlanPcp
                      | OfpfwNwTos
                      | OfpfwAll
                      deriving (Show, Enum)

allWildcards = [ OfpfwInPort, OfpfwDlVlan, OfpfwDlSrc
               , OfpfwDlDst, OfpfwDlType, OfpfwNwProto
               , OfpfwTpSrc, OfpfwTpDst, OfpfwNwSrcShift
               , OfpfwNwSrcBits, OfpfwNwSrcMask, OfpfwNwSrcAll
               , OfpfwDlVlanPcp, OfpfwNwTos, OfpfwAll ]

data OfpFlowModCommand = OfpfcAdd
                       | OfpfcModify
                       | OfpfcModifyStrict
                       | OfpfcDelete
                       | OfpfcDeleteStrict
                       deriving (Show, Enum)

data OfpFlowModFlags = OfpffSendFlowRem
                     | OfpffCheckOverlap
                     | OfpffEmerg
                     deriving (Show, Enum)

data OfpPacketIn =
  OfpPacketIn { piBufferId :: Word32
              , totalLen   :: Word16
              , piInPort   :: Word16
              , reason     :: OfpPacketInReason
              , dat        :: [Word8]
              } deriving (Show)

instance Serialize OfpPacketIn where
  put (OfpPacketIn bufferId _ inPort reason dat) = do
    let dat' = BS.pack dat
    let len  = BS.length dat'
    putWord32be bufferId
    put ((fromIntegral len) :: Word16)
    putWord16be inPort
    putWord8 $ fromIntegral $ fromEnum reason
    putByteString dat'
  get = do
    bufferId <- getWord32be
    totalLen <- getWord16be
    inPort   <- getWord16be
    reason'  <- liftM fromIntegral getWord8
    let reason = toEnum reason' :: OfpPacketInReason
    rem  <- remaining
    -- assert(rem == totalLen)
    dat' <- getByteString rem
    let dat = BS.unpack dat'
    return $ OfpPacketIn bufferId totalLen inPort reason dat

data OfpPacketInReason = OfprNoMatch
                       | OfprAction
                       deriving (Show, Enum)

data OfpPacketOut =
  OfpPacketOut { poBufferId :: Word32
               , poInPort   :: Word16
               , poActions  :: [OfpAction]
               , poDat      :: [Word8]
               } deriving (Show)

instance Serialize OfpPacketOut where
  put (OfpPacketOut bufferId inPort actions dat) = do
    putWord32be bufferId
    putWord16be inPort
    let actions' = runPut (mapM_ put actions)
    let len = BS.length actions'
    put ((fromIntegral len) :: Word16)
    putByteString actions'
    putByteString $ BS.pack dat
  get = do
    bufferId   <- getWord32be
    inPort     <- getWord16be
    actionsLen <- getWord16be
    actionsBS  <- getByteString (fromIntegral actionsLen)
    let actions = readMany actionsBS
    rem  <- remaining
    dat' <- getByteString rem
    let dat = BS.unpack dat'
    return $ OfpPacketOut bufferId inPort actions dat

putOfpErrorMsg ty code = do
  putWord16be ty
  putWord16be $ fromIntegral $ fromEnum code

enumToBitInst :: (Enum a, Bits b) => [a] -> b
enumToBitInst xs = f xs 0
  where
    f xs' w = foldl (\ w x -> w `setBit` fromEnum x) w xs

putOfpMsg :: OfpMsg -> Put
putOfpMsg (OfptHello msg) = putByteString $ BS.pack msg
putOfpMsg (OfptError (OfpetHelloFailed code msg)) = do
  putOfpErrorMsg 0 code
  putByteString $ C.pack msg
putOfpMsg (OfptError (OfpetBadRequest code))    = putOfpErrorMsg 1 code
putOfpMsg (OfptError (OfpetBadAction code))     = putOfpErrorMsg 2 code
putOfpMsg (OfptError (OfpetFlowModFailed code)) = putOfpErrorMsg 3 code
putOfpMsg (OfptError (OfpetPortModFailed code)) = putOfpErrorMsg 4 code
putOfpMsg (OfptError (OfpetQueueOpFailed code)) = putOfpErrorMsg 5 code
putOfpMsg (OfptEchoRequest dat) = putByteString $ BS.pack dat
putOfpMsg (OfptEchoReply dat)   = putByteString $ BS.pack dat
putOfpMsg (OfptVendor)          = putByteString BS.empty
putOfpMsg (OfptFeaturesRequest) = putByteString BS.empty
putOfpMsg (OfptFeaturesReply (OfpSwitchFeatures dip nbuf ntab caps actions ports)) = do
  putWord64be dip
  putWord32be nbuf
  putWord8 ntab
  putWord8 0 -- pad
  putWord8 0 -- pad
  putWord8 0 -- pad
  putWord32be $ enumToBitInst caps
  putWord32be $ enumToBitInst actions
  mapM_ put ports
putOfpMsg (OfptGetConfigRequest)            = putByteString BS.empty
putOfpMsg (OfptGetConfigReply switchConfig) = put switchConfig
putOfpMsg (OfptSetConfig switchConfig)      = put switchConfig
putOfpMsg (OfptPacketIn packetIn)           = put packetIn
putOfpMsg (OfptPacketOut packetOut)         = put packetOut
putOfpMsg (OfptFlowMod flowMod)             = put flowMod
putOfpMsg (OfptBarrierRequest)              = putByteString BS.empty
putOfpMsg (OfptBarrierReply)                = putByteString BS.empty
        
readMany :: (Serialize t) => BS.ByteString -> [t]
readMany bs = case runGet (readMany' [] 0) bs of
  Left err -> error err
  Right t  -> t
  where
    readMany _ 1000 = error "readMany overflow"
    readMany' l n = do
      x   <- get
      rem <- remaining
      if rem > 0
        then readMany' (x:l) (n+1)
        else return (x:l)

bitInstToEnum :: (Enum a, Bits b) => b -> Int -> [a]
bitInstToEnum w width = f 0 []
  where
    f i xs = if i == width
               then xs
               else f (i+1) xs ++ [toEnum i | w `testBit` i]

word16ToEnum word = bitInstToEnum word 16
word32ToEnum word = bitInstToEnum word 32
word64ToEnum word = bitInstToEnum word 64


getOfpMsg (OfpHeader _ ty len _) = 
  case ty of
    0 -> OfptHello       <$> getMsg
    1 -> getOfptError len'
    2 -> OfptEchoRequest <$> getMsg
    3 -> OfptEchoReply   <$> getMsg
    4 -> return OfptVendor
    5 -> return OfptFeaturesRequest
    6 -> getOfptFeaturesReply len'
    7 -> return OfptGetConfigRequest
    8 -> OfptGetConfigReply <$> get
    9 -> OfptSetConfig      <$> get
    10 -> OfptPacketIn      <$> get
    13 -> OfptPacketOut     <$> get
    14 -> OfptFlowMod       <$> get
    18 -> return OfptBarrierRequest
    19 -> return OfptBarrierReply
  where
    len' = fromIntegral len
    getMsg = liftM BS.unpack (getByteString (len' - ofpHdrLen))

getOfptError len = do
  ty   <- liftM fromIntegral getWord16be
  code <- liftM fromIntegral getWord16be
  OfptError <$> case ty of
    0 -> do
      bs <- getByteString (len - ofpHdrLen - 4)
      return $ OfpetHelloFailed (toEnum code :: OfpHelloFailedCode) (C.unpack bs)
    1 -> return $ OfpetBadRequest    (toEnum code :: OfpBadRequestCode)
    2 -> return $ OfpetBadAction     (toEnum code :: OfpBadActionCode)
    3 -> return $ OfpetFlowModFailed (toEnum code :: OfpFlowModFailedCode)
    4 -> return $ OfpetPortModFailed (toEnum code :: OfpPortModFailedCode)
    5 -> return $ OfpetQueueOpFailed (toEnum code :: OfpQueueOpFailedCode)
    _ -> error $ "bad error type"

getOfptFeaturesReply len = do
  dip     <- liftM fromIntegral getWord64be
  nbuf    <- liftM fromIntegral getWord32be
  ntab    <- liftM fromIntegral getWord8
  pad1    <- getWord8
  pad2    <- getWord8
  pad3    <- getWord8
  capBits <- getWord32be
  actBits <- getWord32be
  phyPortsBS <- getByteString (len - ofpHdrLen - 24)
  let caps    = word32ToEnum capBits
  let actions = word32ToEnum actBits
  let ports   = readMany phyPortsBS
  return (OfptFeaturesReply (OfpSwitchFeatures dip nbuf ntab caps actions ports))
  

-- | Socket functions    

peekHdrLen dat =
  case runGet getHdrLen dat of
    Left err -> error err
    Right len -> fromIntegral len
  where
    getHdrLen = do
      OfpHeader _ _ len _ <- get :: Get OfpHeader
      return len

readOfpFrame :: Socket -> IO OfpFrame
readOfpFrame sock = do
  dat <- recvExact ofpHdrLen
  let bytesLeft = (peekHdrLen dat) - ofpHdrLen
  if bytesLeft /= 0
    then do dat' <- recvExact bytesLeft
            case runGet get (BS.append dat dat') of
              Left err -> error err
              Right frame -> return frame
    else do case runGet get dat of
              Left err -> error err
              Right frame -> return frame              
  where
    recvExact bytes = do
      b <- recvExact' bytes BS.empty
      if BS.length b /= fromIntegral bytes
        then error "expected.."
        else return b
    recvExact' bytes buf = do
      dat <- recv sock bytes
      let len = BS.length dat
      if len == 0
        then error "peer closed connection"
        else do let buf' = BS.append buf dat
                if len >= bytes
                  then return buf'
                  else recvExact' (bytes-len) buf'
