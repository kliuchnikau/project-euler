--https://projecteuler.net/problem=25
import Fibonacci
import Digits

firstNDigitsFibonacciNumTermNo :: Int -> Integer
firstNDigitsFibonacciNumTermNo n = 1 + (snd $ head $ reverse $ zip (takeWhile (\fval -> digitsCount(fval) < n) fibonaccies) [2..])

-- firstNDigitsFibonacciNumTermNo 1000
-- (31.29 secs, 46395558216 bytes)
