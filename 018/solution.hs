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

buildProductMatrix :: Matrix.Matrix Integer -> Matrix.Matrix Integer
buildProductMatrix matrix = buildProductMatrix' ((Matrix.nrows matrix) - 1, 1) matrix
  where
    buildProductMatrix' :: (Int,Int) -> Matrix.Matrix Integer -> Matrix.Matrix Integer
    buildProductMatrix' (x,y) matrix
      | x == 0 = matrix
      | y > x = buildProductMatrix' (x-1,1) matrix
      | otherwise = buildProductMatrix' (x,y+1) $ Matrix.setElem (cellProductValue (x,y) matrix) (x,y) matrix
    cellProductValue (x,y) matrix = max productWithLeftLeaf productWithRightLeaf
      where
        productWithLeftLeaf = val (x,y) * val (x+1,y)
        productWithRightLeaf = val (x,y) * val (x+1,y+1)
        val (x,y) = matrix Matrix.! (x,y)

getMaxProduct :: String -> IO Integer
getMaxProduct fileName = do
  fileLines <- readLines fileName
  let basicMatrix = buildMatrix fileLines
  return ((Matrix.! (1,1)) $ buildProductMatrix basicMatrix)

-- getMaxProduct "triangle.txt"
