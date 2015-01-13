module Primes (factors, prime) where
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

--prime calculation
prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime n | even n = False
        | otherwise = (oddFactorsUpto n upperBound) == []
          where
            upperBound = floor (sqrt (fromIntegral n))

oddFactorsUpto :: Int -> Int -> [Int]
oddFactorsUpto n upperBound = oddFactorsUpto' n 3 [] upperBound

oddFactorsUpto' :: Int -> Int -> [Int] -> Int -> [Int]
oddFactorsUpto' num currentFactor result upperBound | reachedUpperBound = result
                                                    | foundNewFactor = oddFactorsUpto' num (currentFactor+2) (currentFactor:result) upperBound
                                                    | otherwise = oddFactorsUpto' num (currentFactor+2) result upperBound
                                                    where
                                                      reachedUpperBound = currentFactor > upperBound
                                                      foundNewFactor = num `mod` currentFactor == 0

