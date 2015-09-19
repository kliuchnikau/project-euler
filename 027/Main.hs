module Main where

import Primes
import Data.List
import Data.Function

type CoefficentsPair = (Int, Int)

quadraticFormula :: CoefficentsPair -> Int -> Int
quadraticFormula (a,b) n = n^2 + a*n + b

producePrimeValue :: CoefficentsPair -> Int -> Bool
producePrimeValue pair = isPrime . abs . quadraticFormula pair

primesStreakLength :: CoefficentsPair -> Int
primesStreakLength pair = length $ takeWhile (producePrimeValue pair) consecutiveValuesOfN
  where
    consecutiveValuesOfN = [0..]

allPossibleCoefficients :: [CoefficentsPair]
allPossibleCoefficients = [ (a,b) | a <- [-limit..limit], b <- [-limit..limit], isPrime b ]
  where
    limit = 1000

coefficientsWithLongestStreak :: CoefficentsPair
coefficientsWithLongestStreak = maximumBy (compare `on` primesStreakLength) allPossibleCoefficients

findProductOfCoefficients :: Int
findProductOfCoefficients = productPair coefficientsWithLongestStreak
  where
    productPair (a,b) = a*b

main = print $ coefficientsWithLongestStreak --findProductOfCoefficients
