-- https://projecteuler.net/problem=4
largestPalindrome :: Int -> Integer
largestPalindrome n = maximum [ x*y | x <-[minNdigitNum..maxNdigitNum], y <- [x..maxNdigitNum], isPalindrome(x*y)]
  where
    minNdigitNum = 10^(n-1)
    maxNdigitNum = 10^(n) - 1

isPalindrome :: Integer -> Bool
isPalindrome n = strN == reverse(strN)
  where
    strN = show n

--largestPalindrome 3
--(0.61 secs, 278846664 bytes)
