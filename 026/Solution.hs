module Solution where

import Primes
import Data.List
import Debug.Trace

{-isPrimitiveRootModulo :: Num a => a -> a -> Bool-}
isPrimitiveRootModulo :: Integer -> Integer -> Bool
isPrimitiveRootModulo base p = [1..p-1] == sort modules
  where
    modules = map (\power -> (base^power) `mod` p) [0..p-2]

findLongestCyclicNumberBefore :: Int -> Integer
findLongestCyclicNumberBefore maxNum = last $ filter (10 `isPrimitiveRootModulo`) allMatchingPrimes
  where
    allMatchingPrimes = map fromIntegral $ takeWhile (< maxNum) primes

main = print $ findLongestCyclicNumberBefore 1000
