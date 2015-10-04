module Main where

lastDigitsOfSumSquares = lastDigits . sum $ map (lastDigits . selfPowers) [1..1000]
  where
    lastDigits = (`mod` 10^10)
    selfPowers x = x^x

main = print $ lastDigitsOfSumSquares
