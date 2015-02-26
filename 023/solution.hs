import Factors
import Data.List
import qualified Data.Set as Set


sumDivisors :: Int -> Int
sumDivisors = sum . deleteLast . factors
  where deleteLast = reverse . tail . reverse

sumDivisorsUnder :: Int -> [(Int, Int)]
sumDivisorsUnder n = map (\num -> (num, sumDivisors num)) [2..(n-1)]

abundantNumsUnder :: Int -> [Int]
abundantNumsUnder n = map fst $ filter isAbundant (sumDivisorsUnder n)
  where
    isAbundant (num,sumDivs) = sumDivs > num

knownMinNeverSumOfAbundants = 28123

allAbundants :: [Int]
allAbundants = abundantNumsUnder knownMinNeverSumOfAbundants

allSumsOf2Abundants :: [Int]
allSumsOf2Abundants = Set.toList $ Set.fromList [x + y | x <- allAbundants, y <- allAbundants, x + y <= knownMinNeverSumOfAbundants]

findNotSumOfAbundants :: Int
findNotSumOfAbundants = sum notSumOfAbundants
  where
    allPossibleNumbers = [1..knownMinNeverSumOfAbundants]
    notSumOfAbundants = allPossibleNumbers \\ allSumsOf2Abundants

-- works 110 sec in ghci. 8 sec in ghc.
--main = print findNotSumOfAbundants
--main = print $ length allAbundants : 6965, 11sec
main = print $ length allSumsOf2Abundants -- 27sec
