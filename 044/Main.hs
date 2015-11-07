module Main where

import QuadraticEquation

isPentagonal :: Int -> Bool
isPentagonal = hasNaturalRoots . pentagonalEq . negate . fromIntegral
  where
    hasNaturalRoots = not . null . naturalRoots
    pentagonalEq :: Double -> QEquation
    pentagonalEq c = QEquation 1.5 (-0.5) c

pentagonals = [ (n*(3*n - 1)) `div` 2 | n <- [1..] ]

pentagonal :: Int -> Int
pentagonal num = pentagonals !! num

findBestPentagonalDifference :: Int
findBestPentagonalDifference = findBestPentagonalDifference' (10^8) 2 [1]

findBestPentagonalDifference' :: Int -> Int -> [Int] -> Int
findBestPentagonalDifference' bestD maxN [] = bestD
findBestPentagonalDifference' bestD maxN minPents = findBestPentagonalDifference' newBestD (maxN+1) possibleMinPentsForNextMax
  where
    maxPent = pentagonal maxN
    newBestD = minimum $ bestD:matchingDs
    matchingDs = map (maxPent -) $ filter doublePenta minPents
    doublePenta minPent = (isPentagonal (maxPent - minPent)) && (isPentagonal (maxPent + minPent))

    possibleMinPentsForNextMax = takeWhile differenceGoodEnough allPossibleMinPents
    allPossibleMinPents = map pentagonal $ [1..maxN]
    differenceGoodEnough minPent = (maxNextPent - minPent) < newBestD
    maxNextPent = pentagonal (maxN+1)

main = print $ findBestPentagonalDifference
