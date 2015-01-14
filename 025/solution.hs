--https://projecteuler.net/problem=25
import Fibonacci
import Digits

firstNDigitsFibonacciNumTermNo :: Int -> Int
firstNDigitsFibonacciNumTermNo n = 2 + (length $ takeWhile (\fval -> digitsCount(fval) < n) fibonaccies)
-- Why 2?
-- In this task first two fibonaccies are 1 and 1, in our library 1 and 2. This is 1.
-- We also find fibonaccies with digits length less than 1000 and we want the next fibonacci after the one with maximum length 999. This is 2.

-- firstNDigitsFibonacciNumTermNo 1000
-- (31.29 secs, 46395558216 bytes)
