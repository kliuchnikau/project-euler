--http://english.stackexchange.com/questions/111765/how-to-write-out-numbers-in-compliance-with-british-usage
import Data.Maybe

knownNumbers = [
  (0, 0),
  (1, length "one"),
  (2, length "two"),
  (3, length "three"),
  (4, length "four"),
  (5, length "five"),
  (6, length "six"),
  (7, length "seven"),
  (8, length "eight"),
  (9, length "nine"),
  (10, length "ten"),
  (11, length "eleven"),
  (12, length "twelve"),
  (13, length "thirteen"),
  (14, length "fourteen"),
  (15, length "fifteen"),
  (16, length "sixteen"),
  (17, length "seventeen"),
  (18, length "eighteen"),
  (19, length "nineteen"),
  (20, length "twenty"),
  (30, length "thirty"),
  (40, length "forty"),
  (50, length "fifty"),
  (60, length "sixty"),
  (70, length "seventy"),
  (80, length "eighty"),
  (90, length "ninety")]

countLetters :: Int -> Int
countLetters num
  | num == 0 = 0
  | num < 20 = known num
  | num < 100 = known ((num `div` 10)*10) + known (num `mod` 10)
  | num < 1000 = known (num `div` 100) + hundred + andWord + countLetters (num `mod` 100)
  | num == 1000 = length "onethousand"
  where
    known num = fromJust $ lookup num knownNumbers
    hundred = length "hundred"
    andWord = if num `mod` 100 /= 0 then length "and" else 0

--sum $ map countLetters [1..1000]
