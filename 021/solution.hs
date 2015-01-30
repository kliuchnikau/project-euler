import Factors
import Data.Maybe

sumDivisors = sum . deleteLast . factors
  where deleteLast = reverse . tail . reverse

sumDivisorsUnder :: Int -> [(Int,Int)]
sumDivisorsUnder n = map (\num -> (num, sumDivisors num)) [2..(n-1)]

amicableNumsUnder :: Int -> [Int]
amicableNumsUnder n = concat $ map (\(amicable1,amicable2) -> [amicable1,amicable2]) $ filter isAmicable allDivisorsSums
  where
    allDivisorsSums = sumDivisorsUnder n
    isAmicable (num, sumDivs) | (sumDivs < 2) || (sumDivs > n) = False
                              | (num >= sumDivs) = False
                              | otherwise = (==num) . fromJust $ lookup sumDivs allDivisorsSums

sumAmicableUnder n = sum $ amicableNumsUnder n
