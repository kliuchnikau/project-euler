-- https://projecteuler.net/problem=20
import Digits
factorial n = product [1..n]
sumOfFactorialDigits n = sum (digits (factorial n))

--sumOfFactorialDigits 100
--648
