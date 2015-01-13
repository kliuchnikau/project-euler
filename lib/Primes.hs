module Primes (factors, factorsUpto, prime) where
import Data.List

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

factorsUpto :: Int -> Int -> [Int]
factorsUpto n upperBound = factorsUpto' n 1 [] upperBound

factorsUpto' :: Int -> Int -> [Int] -> Int -> [Int]
factorsUpto' num currentFactor result upperBound | reachedUpperBound || factorAlreadyInList = sort (nub result)
                                                 | foundNewFactor = factorsUpto' num (currentFactor+1) (currentFactor:result) upperBound
                                                 | otherwise = factorsUpto' num (currentFactor+1) result upperBound
                                                 where
                                                   reachedUpperBound = currentFactor > upperBound
                                                   factorAlreadyInList = elem currentFactor result
                                                   foundNewFactor = num `mod` currentFactor == 0

prime :: Int -> Bool
prime 1 = False
prime n = (factorsUpto n upperBound) == [1]
  where
    upperBound = floor (sqrt (fromIntegral n))
