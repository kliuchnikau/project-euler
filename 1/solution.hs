-- https://projecteuler.net/problem=1
multipleOf3or5 n = (n `mod` 3 == 0) || (n `mod` 5 == 0)
sumOfAllMultipes n = sum (filter multipleOf3or5 [1..n-1])

-- sumOfAllMultipes 1000
-- 233168
