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
data IBitSetArray = IBA {-# UNPACK #-} !Int (Array Int Word64) deriving (Generic, Show)

instance Serialize IBitSetArray

instance NFData IBitSetArray where
  rnf x = x `seq` ()

-- Functions to implement

new :: Int -> IBitSetArray
new n = IBA blocks (listArray (0, blocks) (replicate (blocks + 1) 0))
  where blocks = n `unsafeShiftR` 6

-- | Insert a new integer into the IBitSetArray
insert :: Int -> IBitSetArray -> IBitSetArray
insert i (IBA s a) = IBA s (a // [(block, newWord)])
  where block   = i `unsafeShiftR` 6
        bitIdx  = i .&. 63
        newWord = unsafeSetBit (a ! block) bitIdx

-- | Remove an int from the IBitSetArray
remove :: Int -> IBitSetArray -> IBitSetArray
remove i (IBA s a) = IBA s (a // [(block, newWord)])
  where block   = i `unsafeShiftR` 6
        bitIdx  = i .&. 63
        newWord = unsafeClearBit (a ! block) bitIdx

-- | Return the size of the IBitSetArray. i.e the maximum argument to 'insert' or
-- 'remove'
maxIndex :: IBitSetArray -> Int
maxIndex (IBA s _) = s

-- | Returns true if the given item is contained within the IBitSetArray
contains :: IBitSetArray -> Int -> Bool
contains (IBA _ a) i = testBit (a ! blockIdx) bitIdx
  where blockIdx = i `unsafeShiftR` 6
        bitIdx = i .&. 63

-- | Return the first set bit (lsb) from the IBitSetArray
getFirst :: IBitSetArray -> Int
getFirst ba = fst $ getFirstFromIndex 0 ba

-- | Return the first set bit (lsb) from the IBitSetArray. The first argument
-- allows you to specify the array position element to start searching from.
-- Supplying the starting index if you know that you have already searched part
-- of the set and have not done any insertions.
getFirstFromIndex :: Int -> IBitSetArray -> (Int, Int)
getFirstFromIndex from (IBA s x) = go from
  where
    go :: Int -> (Int, Int)
    go i
      | i > s = (-1, -1)
      | otherwise =
        if x ! i == 0
          then go (i + 1)
          else (countTrailingZeros (x ! i) + (i `unsafeShiftL` 6), i)

-- | Return the intersection of two IBitSetArray so that only elements which are
-- present in both IBitSetArray's are kept. i.e Bitwise and of the BitSets
intersection :: IBitSetArray -> IBitSetArray -> IBitSetArray
intersection (IBA s x) (IBA _ y) = IBA s (listArray (0, s) [((x ! i) .&. (y ! i)) | i <- [0 .. s]])

-- TODO: Check the effeciency of this, not sure bout the: sum (map ...)
-- | Return the intersection of two IBitSetArray so that only elements which are
-- present in both IBitSetArray's are kept. i.e Bitwise and of the BitSets
intersectionPopCount  :: IBitSetArray -> IBitSetArray -> (IBitSetArray, Int)
intersectionPopCount  (IBA s x) (IBA _ y) = (IBA s (listArray (0, s) (map fst newA)), sum (map snd newA))
  where newA = [((x ! i) .&. (y ! i), popCount ((x ! i) .&. (y ! i))) | i <- [0 .. s]]

toList :: IBitSetArray -> [Int]
toList ba@(IBA s x) = foldl inSet [] [(s+1)*64, (s+1)*63 - 1 .. 0]
  where inSet acc i = if contains ba i then i : acc else acc

fromList :: [Int] -> IBitSetArray
fromList xs = foldl (flip insert) (new (maximum xs)) xs
