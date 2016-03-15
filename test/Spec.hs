import Data.BitSetArrayIO
import Data.Array.Base

import Data.Word (Word64)

import Control.Monad

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "BitArrayIO.new" $
    it "Constructs an array of size rounded to the next 64 bit aligment" $ do
      a1 <- new 63
      maxIndex a1  `shouldBe` 0
      a2 <- new 64
      maxIndex a2  `shouldBe` 1
      a3 <- new 150
      maxIndex a3 `shouldBe` 2

  describe "BitArrayIO.insert" $
    it "Inserts an element into the array" $ do
      a1 <- new 50
      insert 0 a1
      s <- showBitSetArray a1
      s `shouldBe` "0,[                                                               1]"

      a2 <- new 50
      insert 4 a2
      s <- showBitSetArray a2
      s `shouldBe` "0,[                                                           10000]"

  -- Can also use a property test here
  describe "BitArrayIO.remove" $
    it "Removes an element from the array" $ do
      a1 <- new 50
      insert 0 a1
      remove 0 a1
      s <- showBitSetArray a1
      s `shouldBe` "0,[                                                               0]"

      a2 <- new 50
      insert 4 a2
      insert 0 a2
      remove 4 a2
      s <- showBitSetArray a2
      s `shouldBe` "0,[                                                               1]"

  describe "BitArrayIO.intersection" $
    it "Performs bitwise and of two arrays" $ do
      let ix1 = [1,4,5,6]
          ix2 = [1,3,5,9]
      a1 <- new 10
      a2 <- new 10
      mapM_ (`insert` a1) ix1
      mapM_ (`insert` a2) ix2
      intersection a1 a2
      s <- showBitSetArray a1
      s `shouldBe` "0,[                                                          100010]"

      let ix1 = [1,4,5,6, 64, 65]
          ix2 = [1,3,5,9, 64]
      a1 <- new 100
      a2 <- new 100
      mapM_ (`insert` a1) ix1
      mapM_ (`insert` a2) ix2
      intersection a1 a2
      s <- showBitSetArray a1
      s `shouldBe` "0,[                                                          100010]\
                   \ 1,[                                                               1]"

  describe "BitArrayIO.intersectionPopCount" $
    it "Performs bitwise and of two arrays and returns the population count" $ do
      let ix1 = [1,4,5,6]
          ix2 = [1,3,5,9]
      a1 <- new 10
      a2 <- new 10
      mapM_ (`insert` a1) ix1
      mapM_ (`insert` a2) ix2
      n <- intersectionPopCount a1 a2
      n `shouldBe` 2

      let ix1 = [1,4,5,6, 64, 65]
          ix2 = [1,3,5,9, 64]
      a1 <- new 100
      a2 <- new 100
      mapM_ (`insert` a1) ix1
      mapM_ (`insert` a2) ix2
      n <- intersectionPopCount a1 a2
      n `shouldBe` 3

  describe "BitArrayIO.getFirst" $
    it "returns the first set element in the array" $ do
      a1 <- new 10
      insert 4 a1
      x <- getFirst a1
      x `shouldBe` 4

      a2 <- new 100
      insert 70 a2
      x <- getFirst a2
      x `shouldBe` 70

  describe "BitArrayIO.copy" $
    it "returns a copy of the BitArray" $ do
      a1 <- new 10
      insert 0 a1
      a2 <- copy a1
      insert 1 a1 -- to ensure it's a copy
      s <- showBitSetArray a2
      s `shouldBe` "0,[                                                               1]"
