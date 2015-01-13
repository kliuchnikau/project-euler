-- https://projecteuler.net/problem=3
import Factors
import Primes

maxPrimeFactor :: Int -> Int
maxPrimeFactor n = maximum [ x | x <- factors n, isPrime x]

-- maxPrimeFactor 600851475143
-- 6857
