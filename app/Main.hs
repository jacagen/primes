module Main (main) where

import Bench (bench)
import qualified BetterSieve (primes)
import Control.Monad (replicateM)
import Data.Map (Map, fromList, (!))
import qualified NaiveSieve (primes)
import System.Environment (getArgs)

impls :: Map String [Integer]
impls = fromList [("NaiveSieve", NaiveSieve.primes), ("BetterSieve", BetterSieve.primes)]

data Arguments = Arguments
  { trials :: Int,
    primes :: Int,
    function :: [Integer]
  }

parseArgs :: [String] -> Arguments
parseArgs [trialsStr, primesStr, functionStr] =
  let fs = impls ! functionStr
      ts = read trialsStr
      ps = read primesStr
   in Arguments {trials = ts, primes = ps, function = fs}
parseArgs _ = error "Bad arguments"

main :: IO ()
main = do
  args' <- getArgs
  let args = parseArgs args'
  results <- replicateM (trials args) (bench (take (primes args) (function args)))
  putStrLn ("Results: " ++ show results)