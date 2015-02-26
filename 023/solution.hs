import Factors
import Data.List

sumDivisors :: Int -> Int
sumDivisors = sum . deleteLast . factors
  where deleteLast = reverse . tail . reverse

sumDivisorsUnder :: Int -> [(Int, Int)]
sumDivisorsUnder n = map (\num -> (num, sumDivisors num)) [2..(n-1)]

abundantNumsUnder :: Int -> [Int]
abundantNumsUnder n = map fst $ filter isAbundant (sumDivisorsUnder n)
  where
    isAbundant (num,sumDivs) = sumDivs > num

findNotSumOfAbundants :: Int
findNotSumOfAbundants = sum notSumOfAbundants
  where
    knownMinNeverSumOfAbundants = 28123
    allPossibleNumbers = [1..knownMinNeverSumOfAbundants]
    allAbundants = abundantNumsUnder knownMinNeverSumOfAbundants
    allSumsOf2Abundants = undefined
    notSumOfAbundants = allPossibleNumbers \\ allSumsOf2Abundants

-- works 110 sec in ghci. 8 sec in ghc.
main = do
  print findNotSumOfAbundants
