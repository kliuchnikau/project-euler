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
buildProductMatrix matrix = buildProductMatrixRow (Matrix.nrows matrix - 1) matrix
  where
    buildProductMatrixRow :: Int -> Matrix.Matrix Integer -> Matrix.Matrix Integer
    buildProductMatrixRow forRow matrix
      | forRow == 0 = matrix
      | otherwise = buildProductMatrixRow (forRow-1) $ buildProductMatrixCells (1, forRow) matrix
    buildProductMatrixCells :: (Int,Int) -> Matrix.Matrix Integer -> Matrix.Matrix Integer
    buildProductMatrixCells (x,y) matrix
      | x > y = matrix
      | otherwise = buildProductMatrixCells (x+1,y) $ Matrix.setElem (cellProductValue (x,y) matrix) (x,y) matrix
    cellProductValue (x,y) matrix = max (val (x,y) * val (x,y+1)) (val (x,y) * val (x+1,y+1))
      where
        val (x,y) = matrix Matrix.! (x,y)


getMaxProduct :: String -> IO Integer
getMaxProduct fileName = do
  fileLines <- readLines fileName
  let basicMatrix = buildMatrix fileLines
  return ((Matrix.! (1,1)) $ buildProductMatrix basicMatrix)

-- getMaxProduct "triangle.txt"
