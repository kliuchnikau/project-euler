module Main where

import Primes
import Data.List

oddCompositeNumbers :: [Int]
oddCompositeNumbers = [ x | x <- [3..], odd x, composite x ]
  where
    composite = not . isPrime

notGoldbach :: Int -> Bool
notGoldbach num = null goldbachRepresentations
  where
    goldbachRepresentations :: [(Int,Int)]
    goldbachRepresentations = [ (prime, matchingSquare prime) | prime <- matchingPrimes, prime + 2*(matchingSquare prime)^2 == num ]

    matchingPrimes :: [Int]
    matchingPrimes = takeWhile (< num) primes

    matchingSquare :: Int -> Int
    matchingSquare prime = floor $ sqrt $ (fromIntegral(num - prime)) / 2

findFirstGoldbachOther = head $ filter notGoldbach oddCompositeNumbers

main = print $ findFirstGoldbachOther
