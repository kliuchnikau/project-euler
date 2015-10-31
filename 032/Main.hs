module Main where

import Digits
import Factors
import Data.List

pandigitalProduct productNum = any isPandigital $ factorsWithout1andSelf productNum
  where
    isPandigital factor = isUniqueCombination (digits factor) (digits $ productNum `div` factor) (digits productNum)
    isUniqueCombination multiplicantDigs multiplierDigs productNumDigs = (sort $ multiplicantDigs ++ multiplierDigs ++ productNumDigs) == [1..9]

findAllPandigitalProducts = filter pandigitalProduct [1000..10000]

main = print $ sum findAllPandigitalProducts
