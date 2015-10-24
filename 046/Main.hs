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
    goldbachRepresentations = [ (prime, square) | prime <- matchingPrimes, square <- matchingSquares prime, prime + 2*square^2 == num ]

    matchingPrimes :: [Int]
    matchingPrimes = takeWhile (< num) primes

    matchingSquares :: Int -> [Int]
    matchingSquares prime = [1..(maximumSquare prime)]
    maximumSquare :: Int -> Int
    maximumSquare prime = floor $ sqrt $ (fromIntegral(num - prime)) / 2

findFirstGoldbachOther = head $ filter notGoldbach oddCompositeNumbers

main = print $ findFirstGoldbachOther
