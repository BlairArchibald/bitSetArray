{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.BitSetArrayIO
(
  -- * BitSet Type
    BitSetArray(..)

  -- * Construction
  , new
  , copy

  -- * Updates
  , insert
  , remove

  -- * Queries
  , maxIndex
  , contains
  , getFirst
  , getFirstFromIndex

  -- * Operations
  , intersection
  , intersectionPopCount

  -- * Mutable/Immutable Interface
  , makeImmutable
  , fromImmutable

  -- * List Interface
  , toList
  , fromList

  -- * Debugging/Printing
  , showBitSetArray
) where

import Data.Array.Base
import Data.Array.IO
import Data.Bits ((.&.), (.|.), unsafeShiftR, countTrailingZeros,
                  popCount, unsafeShiftL, complement, testBit)
import Data.BitSetArray.Util (unsafeSetBit, unsafeClearBit)
import Data.Serialize (Serialize)
import Data.Word (Word64)

import Control.DeepSeq (NFData, rnf)
import Control.Monad

import GHC.Generics (Generic)

import Text.Printf

import qualified Data.IBitSetArray as I

-- Types
-- IOUArray and UArray are unboxed types making them strict and in NF by default (not strictness annotation necessary)
-- | The BitSetArray type provides a strict, unpacked, mutable array of bits
data BitSetArray  =  BA {-# UNPACK #-} !Int (IOUArray Int Word64)

-- Construction
-- | Create a new BitSetArray of a given size. Automatically rounds to the
-- nearest word boundary
new :: Int -> IO BitSetArray
new n = do
  a <- newArray (0, blocks) 0
  return $ BA blocks a
  where blocks = n `unsafeShiftR` 6

-- | Copy a BitSetArray maintaining it's contents
copy :: BitSetArray -> IO BitSetArray
copy (BA s xs) = do
  -- Freeze forces a copy of the array. Not sure if there is an easier way to
  -- copy an array
  cp <- (freeze xs :: IO (UArray Int Word64)) >>= unsafeThaw
  return $ BA s cp

-- Updates

-- | Insert a new integer into the BitSetArray. Does not do a bound check - the
-- first argument must be less than the BitSetArray size. __Use with care__.
-- Insertion of an element which is already in the BitSetArray has no effect.
insert :: Int -> BitSetArray -> IO ()
insert i (BA _ a) = do
  n <- unsafeRead a bx
  unsafeWrite a bx $ unsafeSetBit n bi
  where !bx = i `unsafeShiftR` 6
        !bi = i .&. 63

-- | Remove a new integer into the BitSetArray. Does not do a bound check - the
-- first argument must be less than the BitSetArray size. __Use with care__.
-- Removal of an element which is not in the BitSetArray has no effect.
remove :: Int -> BitSetArray -> IO ()
remove i (BA _ a) = do
  n <- unsafeRead a bx
  unsafeWrite a bx $ unsafeClearBit n bi
  where !bx = i `unsafeShiftR` 6
        !bi = i .&. 63

-- Queries
-- | Return the size of the BitSetArray. i.e the maximum argument to 'insert' or
-- 'remove'
maxIndex :: BitSetArray -> Int
maxIndex (BA s _) = s

-- | Returns true if the given item is contained within the BitSetArray
contains :: BitSetArray -> Int -> IO Bool
contains (BA _ a) i = do
  n <- unsafeRead a bx
  return $ testBit n bi
  where !bx = i `unsafeShiftR` 6
        !bi = i .&. 63

-- | Return the first set bit (lsb) from the BitSetArray
getFirst :: BitSetArray -> IO Int
getFirst ba = fst <$> getFirstFromIndex 0 ba

-- | Return the first set bit (lsb) from the BitSetArray. The first argument
-- allows you to specify the array position element to start searching from.
-- Supplying the starting index if you know that you have already searched part
-- of the set and have not done any insertions.
getFirstFromIndex :: Int -> BitSetArray -> IO (Int, Int)
{-# INLINE getFirstFromIndex #-}
getFirstFromIndex from (BA s x) = go from
  where
    go :: Int -> IO (Int, Int)
    go i
      | i > s = return (-1, -1)
      | otherwise = do
        a <- unsafeRead x i
        if a == 0
          then go (i + 1)
          else return (countTrailingZeros a + (i `unsafeShiftL` 6), i)

-- Operations
-- | Modify the first BitSetArray so that only elements which are present in
-- both BitSetArray's are kept. i.e Bitwise and of the BitSets
intersection :: BitSetArray -> BitSetArray -> IO ()
intersection (BA s x) (BA _ y) = go s
  where
    {-# INLINE go #-}
    go :: Int -> IO ()
    go i
      | i < 0 = return ()
      | otherwise = do
          a <- unsafeRead x i
          b <- unsafeRead y i
          unsafeWrite x i $ a .&. b
          go (i - 1)


-- | Modify the first BitSetArray so that only elements which are present in
-- both BitSetArray's are kept. i.e Bitwise and of the BitSets. Returns the
-- number of bits which were present in both sets (the population count of the
-- modified BitSet).
intersectionPopCount :: BitSetArray -> BitSetArray -> IO Int
intersectionPopCount (BA s x) (BA _ y) =
  go 0 s
  where
    go :: Int -> Int -> IO Int
    go !cnt i
      | i < 0 = return cnt
      | otherwise = do
          a <- unsafeRead x i
          b <- unsafeRead y i
          let ab = a .&. b
              pc = popCount ab
          unsafeWrite x i ab
          go (cnt + pc) (i - 1)

-- Mutable/Immutable Interface
-- | Convert a BitSetArray to an immutable BitSetArray.
-- __Does not perform a copy.__
makeImmutable :: BitSetArray -> IO I.IBitSetArray
makeImmutable (BA s v) = I.IBA s <$> unsafeFreeze v

-- | Convert an immutable BitSetArray to a BitSetArray.
-- __Does not perform a copy.__
fromImmutable :: I.IBitSetArray-> IO BitSetArray
fromImmutable (I.IBA s v) = BA s <$> unsafeThaw v

-- List Interface
-- | Convert a BitSetArray into a list of integers where an integer is in the
-- list if it is set within the BitSetArray
toList :: BitSetArray -> IO [Int]
toList ba@(BA s a) = foldM inSet [] [(s+1)*63, (s+1)*63 - 1  .. 0]
  where inSet acc i = do
          c <- contains ba i
          if c then return (i : acc)
               else return acc

-- | Construct a BitSetArray from a list of integers. Automatically figures out
-- the correct array size by using the maximum value in the list.
fromList :: [Int] -> IO BitSetArray
fromList xs = do
  ba <- new (maximum xs)
  mapM_ (flip insert ba) xs
  return ba

-- Debugging
-- | Print the low-level word representation of the BitSetArray.
showBitSetArray :: BitSetArray -> IO String
showBitSetArray (BA s v) = do
  l <- forM [0 .. s ] $ \i -> do
        x <- unsafeRead v i :: IO Word64
        return $ printf "%d,[%64b]" i x
  return $ unwords l
