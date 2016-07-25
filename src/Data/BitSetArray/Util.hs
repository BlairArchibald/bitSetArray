module Data.BitSetArray.Util where

import Data.Bits ((.&.), (.|.), unsafeShiftR, countTrailingZeros,
                  popCount, unsafeShiftL, complement, testBit)

import Data.Word (Word64)

-- Clear/Set bit without the usual bounds check
unsafeSetBit :: Word64 -> Int -> Word64
{-# INLINE unsafeSetBit #-}
unsafeSetBit w i = w .|. (1 `unsafeShiftL` i)

-- Clear bit without the bounds checks
unsafeClearBit :: Word64 -> Int -> Word64
{-# INLINE unsafeClearBit #-}
unsafeClearBit w i = w .&. complement (1 `unsafeShiftL` i)
