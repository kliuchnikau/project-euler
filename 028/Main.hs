module Main where

sumSquare :: Integer -> Integer
sumSquare 0 = 1
sumSquare squareIndex = 4*topRightCornerElement - (2*squareIndex)*otherCornersCoefficient
  where
    topRightCornerElement = (1 + 2*squareIndex)^2
    otherCornersCoefficient = topLeftCornerCoefficient + bottomLeftCornerCoefficient + bottomRightCornerCoefficient
    topLeftCornerCoefficient = 1
    bottomLeftCornerCoefficient = 2
    bottomRightCornerCoefficient = 3

sumSpinalDiagonals :: Integer -> Integer
sumSpinalDiagonals diagonalLength = sum $ map sumSquare [0..biggestSquerIndex]
  where
    biggestSquerIndex = (diagonalLength-1) `div` 2

main = print $ sumSpinalDiagonals 1001
