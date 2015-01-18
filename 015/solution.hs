-- https://projecteuler.net/problem=15

countPaths :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
countPaths (maxX, maxY) (fromX,fromY) (toX,toY)
  | (fromX > maxX) || (fromY > maxY) = 0
  | (fromX == toX) && (fromY == toY) = 1
  | otherwise = countPaths (maxX, maxY) (fromX+1,fromY) (toX, toY) + countPaths (maxX, maxY) (fromX,fromY+1) (toX, toY)

--countPaths (2,2) (0,0) (2,2)
--countPaths (20, 20) (0,0) (20,20)
