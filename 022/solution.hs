import Data.List.Split

listOfNames :: String -> [String]
listOfNames fileContent = splitOn "," fileContent

readNamesFromFile :: String -> IO [String]
readNamesFromFile fileName = do
  fileContent <- readFile fileName
  return (listOfNames fileContent)

-- readNamesFromFile "names.txt"
