import Factors

sumDivisors :: Int -> Int
sumDivisors = sum . deleteLast . factors
  where deleteLast = reverse . tail . reverse

sumDivisorsUnder :: Int -> [(Int, Int)]
sumDivisorsUnder n = map (\num -> (num, sumDivisors num)) [2..(n-1)]

abundantNumsUnder :: Int -> [Int]
abundantNumsUnder n = map fst $ filter isAbundant (sumDivisorsUnder n)
  where
    isAbundant (num,sumDivs) = sumDivs > num

findNotSumOfAbundantsUnder :: Int
findNotSumOfAbundantsUnder = sum [ x | x <- [1..knownMinNeverSumOfAbundants], notSumOfAbundants x]
  where
    knownMinNeverSumOfAbundants = 28123
    allAbundants = abundantNumsUnder knownMinNeverSumOfAbundants
    notSumOfAbundants = undefined
