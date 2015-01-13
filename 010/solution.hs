-- https://projecteuler.net/problem=10
import Primes

sumPrimesBelow :: Int -> Int
sumPrimesBelow n = sum [ x | x <- [1..n-1], prime x]

-- sumPrimesBelow 2000000
-- 142913828922
-- (519.57 secs, 148427107648 bytes)

otherSumPrimesBelow n = sum [ x | x <- [1..n-1], isPrime x]

primes :: [Integer]
primes = 2 : [ x | x <- [3..], isPrime x]

isPrime :: Integer -> Bool
isPrime x = all (\y -> x `mod` y > 0) (candidates x)
                where
                  candidates z = takeWhile (\p -> p*p <= z) primes

-- 142913828923
-- (26.91 secs, 7311696232 bytes)
