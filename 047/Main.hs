module Main where

import Factors
import Primes

sequenceLength = 4
countDistinctFactors = 4

isMatchingNumber :: Int -> Bool
isMatchingNumber = (countDistinctFactors <=) . length . primeFactors

findFirstSequence = findFirstSequenceNumber' 0 1
findFirstSequenceNumber' :: Int -> Int -> Int
findFirstSequenceNumber' currentLength currentNumber
  | currentLength == sequenceLength = currentNumber - sequenceLength
  | isMatchingNumber currentNumber = findFirstSequenceNumber' (currentLength+1) (currentNumber+1)
  | otherwise = findFirstSequenceNumber' 0 (currentNumber+1)

main = print $ findFirstSequence
