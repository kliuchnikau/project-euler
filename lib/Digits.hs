module Digits (digits, digitsCount) where

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits(n `div` 10) ++ [n `mod` 10]

digitsCount :: Integral a => a -> Int
digitsCount x = length (digits x)
