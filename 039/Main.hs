module Main where

import Data.List
import Data.Function

isIntegralSolution p a = isIntegralB && b > 0 && c > 0
  where
    isIntegralB = (nominatorB `mod` denominatorB == 0)
    nominatorB = p*(2*a - p)
    denominatorB = 2*a - 2*p
    b = nominatorB `div` denominatorB
    c = p - a - b

numSolutions p = (`div` 2) $ length $ filter (isIntegralSolution p) [1..(p `div` 2)]

mostSolutions = fst $ maximumBy (compare `on` snd) $ zipWith (\a b -> (a,b)) [1..1000] $ map numSolutions [1..1000]

main = print $ mostSolutions
