module Primes (factors, primes, isPrime) where
import Data.List

--all factors calculation
factors :: Int -> [Int]
factors n = factors' n 1 []

factors' :: Int -> Int -> [Int] -> [Int]
factors' num currentFactor result | passedNum || factorAlreadyInList = sort (nub result)
                                  | foundNewFactor = factors' num (currentFactor+1) (currentFactor:currentFactorMultiplier:result)
                                  | otherwise = factors' num (currentFactor+1) result
                                  where
                                    passedNum = currentFactor > num
                                    factorAlreadyInList = elem currentFactor result
                                    foundNewFactor = num `mod` currentFactor == 0
                                    currentFactorMultiplier = num `div` currentFactor

primes :: [Int]
primes = 2 : [ x | x <- [3..], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\y -> x `mod` y > 0) (candidates x)
                where
                  candidates z = takeWhile (\p -> p*p <= z) primes

