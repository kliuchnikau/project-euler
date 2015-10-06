module Main where

import Primes
import Digits
import Data.List

isPandigital :: Int -> Bool
isPandigital num = [1..length numDigits] == (sort numDigits)
  where
    numDigits = digits num

biggestPandigitalPrime = head [ x | x <- downwards(7654321), isPandigital x, isPrime x ]
  where
    downwards num = num:downwards(num-1)

main = print $ biggestPandigitalPrime
