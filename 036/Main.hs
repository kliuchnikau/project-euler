module Main where

import Numeric
import Data.Char
import Digits

isPalindrome :: Eq a => [a] -> Bool
isPalindrome digs = digs == reverse digs

findDoubleBasePalindromes = filter (isPalindrome.toBase2) $ filter (isPalindrome.digits) [1..1000000]
  where
    toBase2 num = showIntAtBase 2 intToDigit num ""

main = print $ sum findDoubleBasePalindromes
