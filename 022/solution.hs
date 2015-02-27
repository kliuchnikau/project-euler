import Data.List
import Data.String.Utils

readNamesFromFile :: String -> IO [String]
readNamesFromFile fileName = do
  fileContent <- readFile fileName
  return (listOfNames fileContent)
  where
    listOfNames :: String -> [String]
    listOfNames fileContent = split "," $ replace "\"" "" fileContent

findNamesScores :: [String] -> [String]
findNamesScores namesList = sort namesList

main = do
  namesList <- readNamesFromFile "names.txt"
  return (findNamesScores namesList)
