-- https://projecteuler.net/problem=2
import Fibonacci
import Digits

sumEvenFibonacciNumbers :: Integer -> Integer
sumEvenFibonacciNumbers max = sum (filter even (takeWhile (\fval -> fval <= max) fibonaccies))

--sumEvenFibonacciNumbers 4000000
