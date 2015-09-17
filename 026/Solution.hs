module Solution where

import Primes

isPrimitiveRootModulo :: Int -> Int -> Bool
isPrimitiveRootModulo base p = True

findLongestCyclicNumberBefore :: Int -> Int
findLongestCyclicNumberBefore maxNum = last $ filter (10 `isPrimitiveRootModulo`) allMatchingPrimes
  where
    allMatchingPrimes = takeWhile (\p -> p < maxNum) primes

{-main = print $ findLongestCyclicNumberBefore 1000-}
