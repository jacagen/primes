module NaiveSieve
  ( primes,
  )
where

import Prelude hiding (tail, zipWith, (!!))

sieve :: [Integer] -> [Integer]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
sieve [] = error "Unexpected empty list"

primes :: [Integer]
primes = sieve [2 ..]
