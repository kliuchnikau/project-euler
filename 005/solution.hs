-- https://projecteuler.net/problem=5
import Primes
import Data.List

smallestEvenlyDivisibleBy :: [Int] -> Maybe Int
smallestEvenlyDivisibleBy factors = find divisibleByAllFactors candidates
  where
    divisibleByAllFactors n = all (\factor -> n `mod` factor == 0) factors
    candidates = [ productOfPrimes * x | x <- [1..]]
    productOfPrimes = product primeFactors
    primeFactors = filter isPrime factors

--smallestEvenlyDivisibleBy [1..20]
--(0.01 secs, 7190048 bytes)
