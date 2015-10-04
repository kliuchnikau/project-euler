module Main where

import QuadraticEquation

triangle :: Double -> QEquation
triangle c = QEquation 0.5 0.5 c

pentagonal :: Double -> QEquation
pentagonal c = QEquation 1.5 (-0.5) c

hexagonal :: Double -> QEquation
hexagonal c = QEquation 2.0 (-1.0) c

triangleAndPentagonalAndHexagonal :: Integer -> Bool
triangleAndPentagonalAndHexagonal x = (isPentagonal triangleNumber) && (isHexagonal triangleNumber)
  where
    isPentagonal num = hasNaturalRoots $ pentagonal (negate num)
    isHexagonal num = hasNaturalRoots $ hexagonal (negate num)
    hasNaturalRoots = not . null . naturalRoots
    triangleNumber = fromIntegral $ round $ substituteUnknown basicEquation (fromIntegral x)
    basicEquation = triangle 0.0

main = print $ round $ substituteUnknown (triangle 0.0) (fromIntegral foundNumber)
  where
    foundNumber = head $ filter triangleAndPentagonalAndHexagonal [286..]
