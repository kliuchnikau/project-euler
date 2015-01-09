-- https://projecteuler.net/problem=6
sumOfSquares :: Num a => [a] -> a
sumOfSquares xs = sum (map (\a -> a*a) xs)

squareOfSum :: Num a => [a] -> a
squareOfSum xs = (\x -> x*x) (sum xs)

diffOfSums :: Num a => [a] -> a
diffOfSums xs = (squareOfSum xs) - (sumOfSquares xs)

-- diffOfSums [1..100]
-- 25164150
