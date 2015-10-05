module Main where

import Digits

factorials = 1:[ product [1..x] | x <- [1..]]
factorial n = factorials !! n

biggestPossibleNumber :: Int
biggestPossibleNumber = longestNumWith $ head . reverse $ takeWhile possibleBuildThisLongNumber [1..]
  where
    possibleBuildThisLongNumber digitsNum = digitsNum <= (digitsCount $ longestNumWith digitsNum)
    longestNumWith digitsNum = factorial(9) * digitsNum

allNumbersAreFactorialOfTheirDigits :: [Int]
allNumbersAreFactorialOfTheirDigits = filter factorialOfItsDigits [1..biggestPossibleNumber]
  where
    factorialOfItsDigits num = num == (sum $ map factorial $ digits num)

main = print $ allNumbersAreFactorialOfTheirDigits
