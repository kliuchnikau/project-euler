-- https://projecteuler.net/problem=15
import qualified Data.Matrix as Matrix

countPaths :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
countPaths (maxX, maxY) (toX,toY) = takePathsCount
  where
   takePathsCount (fromX,fromY) = pathCountsCache Matrix.! (fromX,fromY)
   pathCountsCache = Matrix.fromLists [[ countPaths'(x,y) | x <- [1..maxX+1]] | y <- [1..maxY+1]]
   countPaths' (fromX,fromY)
     | (fromX > maxX) || (fromY > maxY) = 0
     | (fromX == toX) && (fromY == toY) = 1
     | otherwise = takePathsCount (fromX+1,fromY) + takePathsCount (fromX,fromY+1)

--countPaths (21, 21) (21,21) (1,1)
