module Main where

import Data.List

numUniquePowers :: Integer -> Integer -> Int
numUniquePowers maxBase maxPower = length . nub $ [base^power | base <- [2..maxBase], power <- [2..maxPower]]

main = print $ numUniquePowers 100 100
