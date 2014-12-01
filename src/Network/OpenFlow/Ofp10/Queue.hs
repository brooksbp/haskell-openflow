--------------------------------------------------------------------------------
-- |
-- Module      :  Network.OpenFlow.Ofp10.Queue
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  experimental
-- Portability :  unknown
--
--------------------------------------------------------------------------------
module Network.OpenFlow.Ofp10.Queue
  (
  -- * Queues
    OfpPacketQueue(..)
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
