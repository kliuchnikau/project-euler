module Solution where

import Primes
import Data.List
import Data.Function

type CoefficentsPair = (Int, Int)

primesStreakLength :: CoefficentsPair -> Int
primesStreakLength (a,b) = indexToCountConvert $ last $ -1 : takeWhile producePrimeValue consecutiveValuesOfN
  where
    consecutiveValuesOfN = [0..]
    producePrimeValue = isPrime . quadraticFormula a b
    quadraticFormula a b n = n^2 + a*n + b
    indexToCountConvert = (1 +)

allPossibleCoefficients :: [CoefficentsPair]
allPossibleCoefficients = [ (a,b) | a <- [-limit..limit], b <- [-limit..limit] ]
  where
    limit = 100

coefficientsWithLongestStreak :: CoefficentsPair
coefficientsWithLongestStreak = maximumBy (compare `on` primesStreakLength) allPossibleCoefficients

{-findProductOfCoefficients :: Int-}
{-findProductOfCoefficients = productPair coefficientsWithLongestStreak-}
  {-where-}
    {-productPair (a,b) = a*b-}

{-main = print $ findProductOfCoefficients-}
