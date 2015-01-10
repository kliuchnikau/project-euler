-- https://projecteuler.net/problem=20
digits 0 = []
digits n = digits(n `div` 10) ++ [n `mod` 10]
factorial n = product [1..n]
sumOfFactorialDigits n = sum (digits (factorial n))

--sumOfFactorialDigits 100
--648
