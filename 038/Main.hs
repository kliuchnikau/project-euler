module Main where

import Digits
import Data.List

potentialPandigitalDigits :: Int -> [Int]
potentialPandigitalDigits num = potentialPandigital' 1 []
  where
    potentialPandigital' multiplier combinedDigits
      | (length combinedDigits) >= 9 = combinedDigits
      | otherwise = potentialPandigital' (multiplier+1) (combinedDigits ++ currentMultiplication)
        where
          currentMultiplication = digits $ num*multiplier

pandigitals :: [Int] -> Bool
pandigitals digitsArray = (sort digitsArray) == [1..9]

findBiggestPandigitalProducts :: Int
findBiggestPandigitalProducts = maximum $ map digitsToNum $ filter pandigitals $ map potentialPandigitalDigits [1..9876]

main = print $ findBiggestPandigitalProducts
