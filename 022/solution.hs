import Data.String.Utils

listOfNames :: String -> [String]
listOfNames fileContent = split "," $ replace "\"" "" fileContent

readNamesFromFile :: String -> IO [String]
readNamesFromFile fileName = do
  fileContent <- readFile fileName
  return (listOfNames fileContent)

-- readNamesFromFile "names.txt"
