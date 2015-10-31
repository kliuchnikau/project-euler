module Main where

import Primes
import Digits
import Data.List

isTruncatable' :: [Int] -> [Int] -> Bool
isTruncatable' [] heads = True
isTruncatable' (x:xs) heads = (isPrimeNum xs) && (isPrimeNum $ reverse (x:heads)) && isTruncatable' xs (x:heads)
  where
    fullHead = x:heads
    isPrimeNum = isPrime . digitsToNum

isTruncatable :: Int -> Bool
isTruncatable num = doubleDigit && (isTruncatable' digs [])
  where
    doubleDigit = num > 10
    digs = digits num

findAllTruncatablePrimes = take 11 $ filter isTruncatable primes

main = print $ sum findAllTruncatablePrimes
