module Main where

import Data.BitSetArrayIO
import Criterion.Main
import qualified Data.IntSet as IS

import Control.Monad

ix1 = [40, 35 , 100, 145, 160]
ix2 = [40, 41 , 34, 145, 160, 150]

main :: IO ()
main = do
  empty <-  new 200
  a1    <-  new 200
  a2    <-  new 200

  mapM_ (`insert` a1) ix1
  mapM_ (`insert` a2) ix2

  let is1 = foldl (flip IS.insert) IS.empty ix1
      is2 = foldl (flip IS.insert) IS.empty ix1

  defaultMain [
    bgroup "bitSetArrayIO" [
       bench "insert_100_Empty" $ nfIO (insert 100 empty)
     , bench "remove_34_a2"     $ nfIO (remove 34 a2)
     , bench "intersection"     $ nfIO (intersection a1 a2)
     , bench "getFirst"         $ nfIO (getFirst a1)
    ]
   , bgroup "IntSet" [
       bench "insert_100_Empty" $ nf (IS.insert 100) IS.empty
     , bench "remove_34_is2"    $ nf (IS.delete 34) is2
     , bench "intersection"     $ nf (IS.intersection is1) is2
     , bench "findMin"          $ nf IS.findMin is1
    ]
   ]
