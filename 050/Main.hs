module Main where

import Primes
import Lists

primesRange :: [Int]
primesRange = take maxPotentialLength primes
  where
    maxPotentialLength = last $ takeWhile sumFitsTheLimit [1..]
    sumFitsTheLimit primesNum = limit > sum (take primesNum primes)
    limit = 1000000

longestConsecutivePrimesSum = sum $ head $ filter (isPrime.sum) possibleWindows
  where
    possibleWindows :: [[Int]]
    possibleWindows = foldr combineWindows [] [0..(length primesRange)]
    combineWindows windowSize otherWindows = otherWindows ++ reverse (windowed windowSize primesRange)

main = print $ longestConsecutivePrimesSum
