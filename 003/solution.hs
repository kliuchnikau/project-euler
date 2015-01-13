-- https://projecteuler.net/problem=3
import Primes

maxPrimeFactor :: Int -> Int
maxPrimeFactor n = maximum [ x | x <- factors n, prime x]

-- maxPrimeFactor 600851475143
-- 6857
