module Solution where

import Primes
import Data.List

primesStreakLength :: (Int, Int) -> Int
primesStreakLength (a,b) = indexToCountConvert $ last $ takeWhile (isPrime . quadraticFormula a b) consecutiveValuesOfN
  where
    indexToCountConvert = (1 +)
    quadraticFormula a b n = n^2 + a*n + b
    consecutiveValuesOfN = [0..]

{-coefficientsWithLongestStreak :: (Int, Int)-}
{-coefficientsWithLongestStreak = maximumBy primesStreakLength [ (a,b) | a <- [-limit..limit], b <- [-limit..limit] ]-}
  {-where-}
    {-limit = 10  1000-}

{-findProductOfCoefficients :: Int-}
{-findProductOfCoefficients = productPair coefficientsWithLongestStreak-}
  {-where-}
    {-productPair (a,b) = a*b-}

{-main = print $ findProductOfCoefficients-}
