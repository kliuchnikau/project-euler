module Main where

import Primes
import Digits

allRotationsOf :: Int -> [Int]
allRotationsOf num =  map rotationStartingAt [0..digitsCount(num)-1]
  where
    rotationStartingAt idx = digitsToNum $ take (digitsCount num) $ drop idx $ cycle (digits num)

isCircular :: Int -> Bool
isCircular num = all isPrime (allRotationsOf num)

main = print $ filter isCircular $ takeWhile (< 1000000) primes
