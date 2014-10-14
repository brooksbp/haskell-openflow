--------------------------------------------------------------------------------
-- |
-- Module      :  Network.OpenFlow.Flow
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
--------------------------------------------------------------------------------
module Network.OpenFlow.Flow
  (
  -- * Flows
    OfpMatch(..)
  , OfpFlowWildcards(..)
  , OfpActionType(..)
  , OfpAction(..)

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
