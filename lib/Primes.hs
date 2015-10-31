module Primes (primes, isPrime) where
import Data.List

primes :: [Int]
primes = 2 : [ x | x <- [3,5..], isPrime x]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = all (\y -> x `mod` y > 0) (candidates x)
                where
                  candidates z = takeWhile (\p -> p*p <= z) primes
