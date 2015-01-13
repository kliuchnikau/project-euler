-- https://projecteuler.net/problem=10
import Primes

sumPrimesBelow :: Int -> Int
sumPrimesBelow n = sum [ x | x <- [1..n-1], prime x]

-- sumPrimesBelow 2000000
-- ???
