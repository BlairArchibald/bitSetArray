import Data.BitSetArrayIO
import qualified Data.IBitSetArray as I
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

  describe "IBitArray.new" $
    it "Constructs an array of size rounded to the next 64 bit aligment" $ do
      I.maxIndex (I.new 63) `shouldBe` 0
      I.maxIndex (I.new 64) `shouldBe` 1
      I.maxIndex (I.new 150) `shouldBe` 2

  -- I don't need show if I get an Eq instance
  describe "IBitArray.insert" $
    it "Inserts an element into the array" $ do
      show (I.insert 0 (I.new 50)) `shouldBe` "IBA 0 (array (0,0) [(0,1)])"
      show (I.insert 1 (I.new 50)) `shouldBe` "IBA 0 (array (0,0) [(0,2)])"
      show (I.insert 64 (I.new 70)) `shouldBe` "IBA 1 (array (0,1) [(0,0),(1,1)])"

  -- Could also do properties here
  describe "IBitArray.remove" $
    it "Remove an element from the array" $ do
      show (I.remove 0 (I.insert 0 (I.new 50))) `shouldBe` "IBA 0 (array (0,0) [(0,0)])"
      show (I.remove 1 (I.insert 1 (I.new 50))) `shouldBe` "IBA 0 (array (0,0) [(0,0)])"
      show (I.remove 64 (I.insert 64 (I.new 70))) `shouldBe` "IBA 1 (array (0,1) [(0,0),(1,0)])"

  describe "IBitArray.intersection" $
    it "Performs bitwise and of two arrays" $ do
      let a1 = I.fromList [1,4,5,6]
          a2 = I.fromList [1,2,3,7]
          a3 = I.fromList [1,4,3,7]
      show (I.intersection a1 a2) `shouldBe` "IBA 0 (array (0,0) [(0,2)])"
      show (I.intersection a1 a3) `shouldBe` "IBA 0 (array (0,0) [(0,18)])"

  describe "IBitArray.getFirst" $
    it "returns the first set element in the bitset" $ do
      I.getFirst (I.fromList [5]) `shouldBe` 5

  describe "IBitArray.contains" $
    it "Returns true if an element is in the bitset" $ do
      I.contains (I.fromList [5]) 5 `shouldBe` True
      I.contains (I.fromList [5]) 3 `shouldBe` False
