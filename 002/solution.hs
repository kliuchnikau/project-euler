-- https://projecteuler.net/problem=2
fibonaccies :: [Integer]
fibonaccies = 1:2:[ (fibonacci x) + (fibonacci (x+1)) | x <- [0..]]

fibonacci :: Int -> Integer
fibonacci num = fibonaccies !! num

sumEvenFibonacciNumbers :: Integer -> Integer
sumEvenFibonacciNumbers max = sum (filter even (takeWhile (\fval -> fval <= max) fibonaccies))

--sumEvenFibonacciNumbers 4000000
