-- https://projecteuler.net/problem=6
square :: Num a => a -> a
square x = x*x

sumOfSquares :: Num a => [a] -> a
sumOfSquares xs = sum (map square xs)

squareOfSum :: Num a => [a] -> a
squareOfSum xs = square (sum xs)

diffOfSums :: Num a => [a] -> a
diffOfSums xs = (squareOfSum xs) - (sumOfSquares xs)

-- diffOfSums [1..100]
