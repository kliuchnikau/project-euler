-- https://projecteuler.net/problem=10
import Primes

sumPrimesBelow :: Int -> Int
sumPrimesBelow n = sum [ x | x <- [1..n-1], prime x]

prime :: Int -> Bool
prime 1 = False
prime 2 = True
prime n | even n = False
        | otherwise = (oddFactorsUpto n upperBound) == []
          where
            upperBound = floor (sqrt (fromIntegral n))

oddFactorsUpto :: Int -> Int -> [Int]
oddFactorsUpto n upperBound = oddFactorsUpto' n 3 [] upperBound

oddFactorsUpto' :: Int -> Int -> [Int] -> Int -> [Int]
oddFactorsUpto' num currentFactor result upperBound | reachedUpperBound = result
                                                    | foundNewFactor = oddFactorsUpto' num (currentFactor+2) (currentFactor:result) upperBound
                                                    | otherwise = oddFactorsUpto' num (currentFactor+2) result upperBound
                                                    where
                                                      reachedUpperBound = currentFactor > upperBound
                                                      foundNewFactor = num `mod` currentFactor == 0

-- sumPrimesBelow 2000000
-- 142913828922
-- (519.57 secs, 148427107648 bytes)

otherSumPrimesBelow n = sum [ x | x <- [1..n-1], isPrime x]

-- 142913828923
-- (26.91 secs, 7311696232 bytes)
