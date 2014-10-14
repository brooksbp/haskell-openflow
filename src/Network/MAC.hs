--------------------------------------------------------------------------------
-- |
-- Module      :  Network.MAC
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-- Adapted from Network.Info
--------------------------------------------------------------------------------
module Network.MAC
  ( MAC(..)
  ) where

import Control.Applicative
import Data.Binary
import Text.Printf

data MAC = MAC !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
         deriving (Eq, Ord)

instance Show MAC where
  show (MAC a b c d e f) = printf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f

instance Binary MAC where
  put (MAC a b c d e f) = do put a ; put b ; put c ; put d ; put e ; put f
  get = MAC <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
