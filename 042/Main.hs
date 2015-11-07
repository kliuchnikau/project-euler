module Main where

import Data.List
import Data.Char
import Data.String.Utils

readWordsFromFile :: String -> IO [String]
readWordsFromFile fileWord = do
  fileContent <- readFile fileWord
  return (listOfWords fileContent)
  where
    listOfWords :: String -> [String]
    listOfWords fileContent = split "," $ replace "\"" "" fileContent

alphabeticalScore :: String -> Int
alphabeticalScore word = sum $ map letterIndex word
  where
    letterIndex char = ord char - letterAindex + 1
    letterAindex = ord 'A'

triangles = [ (n*(n+1)) `div` 2 | n <- [1..] ]

isTriangle :: Int -> Bool
isTriangle score = score == potentialMatchingTriangle
  where
    potentialMatchingTriangle = head $ dropWhile (< score) triangles

main = do
  wordsList <- readWordsFromFile "words.txt"
  let triangleScores = filter isTriangle $ map alphabeticalScore wordsList
  return (length triangleScores)
