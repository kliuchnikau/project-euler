module Main where

import Digits

ds :: [Int]
ds = foldr (++) [] (map digits [1..])

d :: Int -> Int
d num = ds !! (num-1)

main = print $ product [d 1, d 10, d 100, d 1000, d 10000, d 100000, d 1000000]
