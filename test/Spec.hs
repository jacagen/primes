{-# OPTIONS_GHC -Wno-type-defaults #-}

module Spec (Expectation, spec) where

import qualified BetterSieve (primes)
import BetterSieve.Internal (updateAppendMap)
import Data.List (sort)
import Data.Map (Map, empty, fromList)
import qualified Data.Map as Map (map)
import qualified NaiveSieve (primes)
import qualified Primes (primes)
import Test.Hspec

spec :: Spec
spec = do
  describe "updateAppendMap" $ do
    it "adds a key-value pair to an empty map" $ do
      let result = updateAppendMap "key1" 42 empty
      result `compareMaps` fromList [("key1", [42])]

    it "appends a value to an existing key's list" $ do
      let initialMap = fromList [("key1", [42])]
      let result = updateAppendMap "key1" 99 initialMap
      result `compareMaps` fromList [("key1", [42, 99])]

    it "adds a new key-value pair to a non-empty map" $ do
      let initialMap = fromList [("key1", [42])]
      let result = updateAppendMap "key2" 99 initialMap
      result `compareMaps` fromList [("key1", [42]), ("key2", [99])]

    it "appends multiple values to an existing key in sequence" $ do
      let initialMap = fromList [("key1", [42])]
      let result = updateAppendMap "key1" 99 (updateAppendMap "key1" 21 initialMap)
      result `compareMaps` fromList [("key1", [42, 21, 99])]

    it "handles an empty value list gracefully" $ do
      let initialMap = fromList [("key1", [])]
      let result = updateAppendMap "key1" 42 initialMap
      result `compareMaps` fromList [("key1", [42])]

    it "works with different types of keys and values" $ do
      let initialMap = fromList [(1, ["a"])]
      let result = updateAppendMap 1 "b" (updateAppendMap 2 "c" initialMap)
      result `compareMaps` fromList [(1, ["a", "b"]), (2, ["c"])]

  describe "primes" $ do
    it "First many primes better sieve" $ do
      Primes.primes `shouldBe` (take (length Primes.primes) BetterSieve.primes)
    it "First many primes naive sieve" $ do
      -- Too slow to be a unit test
      Primes.primes `compareLists` (take (length Primes.primes) NaiveSieve.primes)
    it "First primes naieve sieve" $ do
      -- Too slow to be a unit test
      [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] `compareLists` (take 10 NaiveSieve.primes)
    it "First primes better sieve" $ do
      -- Too slow to be a unit test
      [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] `compareLists` (take 10 BetterSieve.primes)

compareMaps :: (Eq a, Show a, Show b, Ord b) => Map a [b] -> Map a [b] -> Expectation
compareMaps m1 m2 =
  (Map.map sort m1) `shouldBe` (Map.map sort m2)

compareLists :: (Show a, Ord a) => [a] -> [a] -> Expectation
compareLists xs ys = do
  length xs `shouldBe` length ys
  xs `shouldBe` ys
