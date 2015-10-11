module Factors (factors,factorsWithout1andSelf,primeFactors) where
import Data.List
import Primes

primeFactors :: Int -> [Int]
primeFactors n = filter isFactor candidates
  where
    candidates = takeWhile (<= (n `div` 2)) primes
    isFactor factor = n `mod` factor == 0

factorsWithout1andSelf :: Int -> [Int]
factorsWithout1andSelf n = factors' n 2 []

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
