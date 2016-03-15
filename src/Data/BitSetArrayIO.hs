{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.BitSetArrayIO
(
  -- * BitSet Type
    BitSetArray

  -- * Construction
  , new
  , copy

  -- * Updates
  , insert
  , remove

  -- * Queries
  , maxIndex
  , getFirst
  , getFirstFromIndex

  -- * Operations
  , intersection
  , intersectionPopCount

  -- * Debugging/Printing
  , showBitSetArray
) where

import Data.Array.Base
import Data.Array.IO

import Data.Bits ((.&.), (.|.), unsafeShiftR, countTrailingZeros, popCount, unsafeShiftL, complement)

import Data.Word (Word64)
import Control.Monad

import Text.Printf

-- Types
data BitSetArray = BA {-# UNPACK #-} !Int (IOUArray Int Word64)

-- Construction
new :: Int -> IO BitSetArray
new n = do
  a <- newArray (0, blocks) 0
  return $ BA blocks a
  where blocks = n `unsafeShiftR` 6

copy :: BitSetArray -> IO BitSetArray
copy (BA s xs) = do
  cp <- (freeze xs :: IO (UArray Int Word64)) >>= unsafeThaw
  return $ BA s cp

-- Updates

insert :: Int -> BitSetArray -> IO ()
insert i (BA _ a) = do
  n <- unsafeRead a bx
  unsafeWrite a bx $ unsafeSetBit n bi
  where !bx = i `unsafeShiftR` 6
        !bi = i .&. 63

remove :: Int -> BitSetArray -> IO ()
remove i (BA _ a) = do
  n <- unsafeRead a bx
  unsafeWrite a bx $ unsafeClearBit n bi
  where !bx = i `unsafeShiftR` 6
        !bi = i .&. 63

-- Clear/Set bit without the usual bounds check
unsafeSetBit :: Word64 -> Int -> Word64
{-# INLINE unsafeSetBit #-}
unsafeSetBit w i = w .|. (1 `unsafeShiftL` i)

-- Clear bit without the bounds checks
unsafeClearBit :: Word64 -> Int -> Word64
{-# INLINE unsafeClearBit #-}
unsafeClearBit w i = w .&. complement (1 `unsafeShiftL` i)

-- Queries
maxIndex :: BitSetArray -> Int
maxIndex (BA s _) = s

getFirst :: BitSetArray -> IO Int
getFirst ba = fst <$> getFirstFromIndex 0 ba

-- Useful to be able to provide a starting index if you know that you have
-- already searched part of the set and haven't done any insertions. Java BitSet
-- has a similar API.
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


-- Debugging
showBitSetArray :: BitSetArray -> IO String
showBitSetArray (BA s v) = do
  l <- forM [0 .. s ] $ \i -> do
        x <- unsafeRead v i :: IO Word64
        return $ printf "%d,[%64b]" i x
  return $ unwords l
