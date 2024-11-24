module Main (main) where

import qualified NaiveSieve (primes)
import qualified BetterSieve (primes)
import Bench (bench)
import Control.Monad (replicateM)

primeCount :: Int
primeCount = 1000000


main :: IO ()
main = do
    --naiveResults <- replicateM 1 (bench (take primeCount NaiveSieve.primes))
    betterResults <- replicateM 1 (bench (take primeCount BetterSieve.primes))
    --putStrLn ("Naive results (" ++ show primeCount ++ "): " ++ show naiveResults)
    putStrLn ("Better results (" ++ show primeCount ++ "): " ++ show betterResults)

