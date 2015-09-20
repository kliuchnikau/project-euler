module Main where

import Digits

power = 5

maxNumDigits :: Int
maxNumDigits = last $ takeWhile canProduceLongEnoughNumber [2..]
  where
    canProduceLongEnoughNumber numDigits = (maxNumDigestWeCanProduce numDigits) >= numDigits
    maxNumDigestWeCanProduce numDigits = digitsCount (numDigits * 9^power)

allPossibleNumbers :: [Int]
allPossibleNumbers = [2..maxPossibleNumber]
  where
    maxPossibleNumber = 10^maxNumDigits - 1

powerOfDigitsEqNumber :: Int -> Bool
powerOfDigitsEqNumber num = num == (sumPowerDigits num)
  where
    sumPowerDigits = sum . map (^power) . digits

main = print $ sum $ filter powerOfDigitsEqNumber allPossibleNumbers
