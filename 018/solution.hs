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
buildProductMatrix = undefined

getMaxProduct :: String -> IO Integer
getMaxProduct fileName = do
  fileLines <- readLines fileName
  let basicMatrix = buildMatrix fileLines
  return ((Matrix.! (1,1)) $ buildProductMatrix basicMatrix)

-- getMaxProduct "triangle.txt"
