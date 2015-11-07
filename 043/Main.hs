module Main where

import Digits
import Lists
import Primes
import Data.List
import Debug.Trace

findPandigitalsWithDivisibility = map digitsToNum $ filter (\pd -> notStartingWith0 pd && subStringDivisible pd) allPandigitals
  where
    allPandigitals = permutations [0..9] -- [digits 1406357289]
    notStartingWith0 = not . (== 0) . head
    subStringDivisible num = all divisible $ zip (allWindows num) primes
    allWindows = windowed 3 . tail
    divisible (window,prime) = ((digitsToNum window) `mod` prime) == 0

main = print $ sum findPandigitalsWithDivisibility
