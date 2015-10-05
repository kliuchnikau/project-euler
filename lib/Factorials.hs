module Factorials where

factorials :: Integral a => [a]
factorials = 1:[ product [1..x] | x <- [1..]]

factorial :: Integral a => Int -> a
factorial n = factorials !! n
