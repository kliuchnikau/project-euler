module Solution where

import Primes
import Data.List

isPrimitiveRootModulo :: Int -> Int -> Bool
isPrimitiveRootModulo base p = [1..base-1] == modules
  where
    modules = map (\power -> (p^power) `mod` base) [0..base-2]

findLongestCyclicNumberBefore :: Int -> Int
findLongestCyclicNumberBefore maxNum = last $ filter (10 `isPrimitiveRootModulo`) allMatchingPrimes
  where
    allMatchingPrimes = takeWhile (\p -> p < maxNum) primes

{-main = print $ findLongestCyclicNumberBefore 1000-}
