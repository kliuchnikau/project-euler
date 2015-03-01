permutationsOf :: [Int] -> Int
permutationsOf digits = product [1..size]
  where
    size = length digits

findLexicographicalPermutation :: Int -> [Int] -> [Int]
findLexicographicalPermutation number availableDigits
  | length availableDigits == 1 = availableDigits
  | otherwise = [foundDigit] ++ findLexicographicalPermutation reducedNumber outstandingAvailableDigits
  where
    permutationsOfOutstandingDigits = permutationsOf (tail availableDigits)
    reducedNumber = number `mod` permutationsOfOutstandingDigits
    foundDigit = availableDigits !! (number `div` permutationsOfOutstandingDigits)
    outstandingAvailableDigits = filter (\digit -> digit /= foundDigit) availableDigits

main = return (findLexicographicalPermutation 999999 [0..9])
