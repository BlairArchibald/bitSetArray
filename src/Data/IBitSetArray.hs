{-# LANGUAGE DeriveGeneric #-}
module Data.IBitSetArray where

import Control.DeepSeq (NFData, rnf)
import Control.Monad

import Data.Array.IArray
import Data.Bits ((.&.), (.|.), unsafeShiftR, countTrailingZeros,
                  popCount, unsafeShiftL, complement, testBit)

import Data.BitSetArray.Util (unsafeSetBit, unsafeClearBit)

import Data.Serialize (Serialize)
import Data.Word (Word64)

import GHC.Generics (Generic)

-- | The IBitSetArray type provides a unpacked, lazy, immutable array of bits
data IBitSetArray = IBA {-# UNPACK #-} !Int (Array Int Word64) deriving (Generic)

instance Serialize IBitSetArray

instance NFData IBitSetArray where
  rnf x = x `seq` ()

-- Functions to implement

new :: Int -> IBitSetArray
new n = IBA blocks (listArray (0, blocks) (replicate blocks 0))
  where blocks = n `unsafeShiftR` 6

insert :: Int -> IBitSetArray -> IBitSetArray
insert i (IBA s a) = IBA s (a // [(block, newWord)])
  where block   = i `unsafeShiftR` 6
        bitIdx  = i .&. 63
        newWord = unsafeSetBit (a ! block) bitIdx

-- , remove
-- , maxIndex
-- , contains
-- , getFirst
-- , getFirstFromIndex
-- , intersection
-- , intersectionPopCount
-- , makeMutable
-- , fromMutable
-- , toList
-- , fromList
-- , showBitSetArray
