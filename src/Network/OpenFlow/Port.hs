--------------------------------------------------------------------------------
-- |
-- Module      :  Network.OpenFlow.Port
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
--------------------------------------------------------------------------------
module Network.OpenFlow.Port
  (
  -- * Ports
    OfpPhyPort(..)
  , OfpPortConfig(..)
  , OfpPortState(..)
  , OfpPort
  , OfpPortFeatures(..)

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
