module Factors (factors) where
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
