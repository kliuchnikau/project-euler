--https://projecteuler.net/problem=25
import Fibonacci
import Digits

firstNDigitsFibonacciNumTermNo :: Int -> Int
firstNDigitsFibonacciNumTermNo n = 2 + (length $ takeWhile (\fval -> digitsCount(fval) < n) fibonaccies)

-- firstNDigitsFibonacciNumTermNo 1000
-- (31.29 secs, 46395558216 bytes)
