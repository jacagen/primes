module BetterSieve
  ( primes,
  )
where

import BetterSieve.Internal (updateAppendMap)
import Data.Map (Map)
import qualified Data.Map (empty, lookup)

primes :: [Integer]
primes = sieve [2 ..] Data.Map.empty

sieve :: [Integer] -> Map Integer [Integer] -> [Integer]
sieve [] _ = undefined
sieve (n : ns) m =
  case Data.Map.lookup n m of
    Nothing -> n : sieve ns (updateAppendMap (n + n) n m)
    Just l ->
      sieve
        ns
        ( foldr
            (\x m' -> updateAppendMap (n + x) x m')
            m
            l
        )
