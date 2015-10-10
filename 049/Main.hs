module Main where

import Primes
import Digits
import Data.List

increase = 3330

primePermutations :: [Int]
primePermutations = filter isMatchingNum candidates
  where
    candidates = [1000..maxPossibleNum]
    maxPossibleNum = 10000 - (increase*2)
    isMatchingNum num = isPrimePermutationTrio (num,num+increase,num+2*increase)
    isPrimePermutationTrio (a,b,c) = (isPrime a) && (isPrime b) && (isPrime c) && isPermutationInTrio (a,b,c)
    isPermutationInTrio (a,b,c) = (sortedDigits a == sortedDigits b) && (sortedDigits a == sortedDigits c)
    sortedDigits = sort.digits

main = print $ map (\x -> [x,x+increase,x+2*increase]) primePermutations
