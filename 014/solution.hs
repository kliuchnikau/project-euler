-- https://projecteuler.net/problem=15
import Data.List
import Data.Function

collatzLengths :: [Integer]
collatzLengths = [findCollatzLength x | x <-[1..]]
  where
    findCollatzLength n
      | n == 1 = 1
      | even n = 1 + findCollatzLength(n `div` 2)
      | otherwise = 1 + findCollatzLength(n * 3 + 1)

longestLength upto = maximumBy (compare `on` fst) $ zip (take upto collatzLengths) [1..]
--The solution with memoization is significantly slower in ghci
--longestLength 999999
