module Main where

import Digits
import Factorials

biggestPossibleNumber :: Int
biggestPossibleNumber = longestNumWith $ head . reverse $ takeWhile possibleBuildThisLongNumber [1..]
  where
    possibleBuildThisLongNumber digitsNum = digitsNum <= (digitsCount $ longestNumWith digitsNum)
    longestNumWith digitsNum = digitsNum * factorial(9)

allNumbersAreFactorialOfTheirDigits :: [Int]
allNumbersAreFactorialOfTheirDigits = filter factorialOfItsDigits [1..biggestPossibleNumber]
  where
    factorialOfItsDigits num = num == (sum $ map factorial $ digits num)

main = print $ allNumbersAreFactorialOfTheirDigits
