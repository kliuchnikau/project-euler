-- read triangle into matrix
import qualified Data.Matrix as Matrix
import Data.List.Split

readLines :: String -> IO [String]
readLines fileName = do
  content <- readFile fileName
  return (lines content)

adjustListsToSameLength :: [[Integer]] -> [[Integer]]
adjustListsToSameLength lists = map (\ints -> take sameLength (ints ++ infiniteZeros)) lists
  where
    infiniteZeros = repeat 0
    sameLength = maximum (map length lists)

buildMatrix :: [String] -> Matrix.Matrix Integer
buildMatrix input = Matrix.fromLists $ adjustListsToSameLength $ map listOfInts input
  where
    listOfInts :: String -> [Integer]
    listOfInts str = map (\intStr -> read intStr :: Integer) $ splitOn " " str

buildSumMatrix :: Matrix.Matrix Integer -> Matrix.Matrix Integer
buildSumMatrix matrix = buildSumMatrix' ((Matrix.nrows matrix) - 1, 1) matrix
  where
    buildSumMatrix' :: (Int,Int) -> Matrix.Matrix Integer -> Matrix.Matrix Integer
    buildSumMatrix' (x,y) matrix
      | x == 0 = matrix
      | y > x = buildSumMatrix' (x-1,1) matrix
      | otherwise = buildSumMatrix' (x,y+1) $ Matrix.setElem (cellSumValue (x,y) matrix) (x,y) matrix
    cellSumValue (x,y) matrix = max sumWithLeftLeaf sumWithRightLeaf
      where
        sumWithLeftLeaf = val (x,y) + val (x+1,y)
        sumWithRightLeaf = val (x,y) + val (x+1,y+1)
        val (x,y) = matrix Matrix.! (x,y)

getMaxSum :: String -> IO Integer
getMaxSum fileName = do
  fileLines <- readLines fileName
  let basicMatrix = buildMatrix fileLines
  return ((Matrix.! (1,1)) $ buildSumMatrix basicMatrix)

-- getMaxSum "triangle.txt"
