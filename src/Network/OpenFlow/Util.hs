--------------------------------------------------------------------------------
-- |
-- Module      :  Network.OpenFlow.Util
-- Copyright   :  (C) 2014 Brian Brooks
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Brian Brooks <brooks.brian@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
--------------------------------------------------------------------------------
module Network.OpenFlow.Util
  (
    enumToBits
  , bitsToEnum
  , word16ToEnum
  , word32ToEnum
  , word64ToEnum

  ) where

import Data.Bits

-- Note [bitmasks and enums]
--
-- If a C enum doesn't specify values and the implied ordering is consequetive
-- sequential then just define a standard Haskell data enumeration type
-- deriving Enum, the same ordering will be applied when you fromEnum.
--
-- If a C enum is not this, use a table to define the values and create the
-- instance ourself.
--
-- Use enumToBits and bitsToEnum for serializing both.

-- | Create a 'bitmap' from an Enum
enumToBits :: (Enum a, Bits b, Num b) => [a] -> b
enumToBits x = f x 0
  where
    f x' w = foldl (\l r -> l `setBit` fromEnum r) w x

-- | Reverse
bitsToEnum :: (Enum a, Bits b) => Int -> b -> [a]
bitsToEnum width w = f 0 []
  where
    f i xs = if i == width
               then xs
               else f (i+1) xs ++ [toEnum i | w `testBit` i]

word16ToEnum, word32ToEnum, word64ToEnum :: (Enum a, Bits b) => b -> [a]
word16ToEnum = bitsToEnum 16
word32ToEnum = bitsToEnum 32
word64ToEnum = bitsToEnum 64
