module Fibonacci (fibonaccies, fibonacci) where

fibonaccies :: [Integer]
fibonaccies = 1:2:[ (fibonacci x) + (fibonacci (x+1)) | x <- [0..]]

fibonacci :: Int -> Integer
fibonacci num = fibonaccies !! num
