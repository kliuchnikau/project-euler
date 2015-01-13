module Primes (factors, prime) where
import Data.List

factors :: Int -> [Int]
factors n = factors' n 1 []

factors' :: Int -> Int -> [Int] -> [Int]
factors' num currentFactor result | passedNum = sort (nub result)
                                  | factorAlreadyInList = sort (nub result)
                                  | foundNewFactor = factors' num (currentFactor+1) (currentFactor:currentFactorMultiplier:result)
                                  | otherwise = factors' num (currentFactor+1) result
                                  where
                                    passedNum = currentFactor > num
                                    factorAlreadyInList = elem currentFactor result
                                    foundNewFactor = num `mod` currentFactor == 0
                                    currentFactorMultiplier = num `div` currentFactor

prime :: Int -> Bool
prime x = (factors x) == [1,x]
