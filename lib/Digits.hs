module Digits (digits) where

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits(n `div` 10) ++ [n `mod` 10]
