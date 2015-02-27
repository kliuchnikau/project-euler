import Data.List
import Data.Char
import Data.String.Utils

readNamesFromFile :: String -> IO [String]
readNamesFromFile fileName = do
  fileContent <- readFile fileName
  return (listOfNames fileContent)
  where
    listOfNames :: String -> [String]
    listOfNames fileContent = split "," $ replace "\"" "" fileContent

findNamesScores :: [String] -> [Int]
findNamesScores namesList = map nameScore $ zip (sort namesList) [1..]
  where
    nameScore (name, index) = (alphabeticalScore name) * index

alphabeticalScore :: String -> Int
alphabeticalScore name = sum $ map letterIndex name
  where
    letterIndex char = ord char - letterAindex + 1
    letterAindex = ord 'A'

main = do
  namesList <- readNamesFromFile "names.txt"
  let nameScores = findNamesScores namesList
  return (sum nameScores)
