-- https://projecteuler.net/problem=3
import Data.List
factors :: Int -> [Int]
factors n = getNextFactor n 1 []

getNextFactor :: Int -> Int -> [Int] -> [Int]
getNextFactor num currentFactor result | passedNum = sort (nub result)
                                       | factorAlreadyInList = sort (nub result)
                                       | foundNewFactor = getNextFactor num (currentFactor+1) (currentFactor:currentFactorMultiplier:result)
                                       | otherwise = getNextFactor num (currentFactor+1) result
                                       where
                                         passedNum = currentFactor > num
                                         factorAlreadyInList = elem currentFactor result
                                         foundNewFactor = num `mod` currentFactor == 0
                                         currentFactorMultiplier = num `div` currentFactor

prime :: Int -> Bool
prime x = (factors x) == [1,x]

maxPrimeFactor :: Int -> Int
maxPrimeFactor n = maximum [ x | x <- factors n, prime x]

-- maxPrimeFactor 600851475143
-- 6857
