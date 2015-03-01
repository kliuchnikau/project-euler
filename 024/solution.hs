permutationsOf :: [Int] -> Int
permutationsOf digits = product [1..size]
  where
    size = length digits

findLexicographicalPermutation :: Int -> [Int] -> [Int]
findLexicographicalPermutation number availableDigits
  | length availableDigits == 1 = availableDigits
  | otherwise = [foundDigit] ++ findLexicographicalPermutation reducedNumber outstandingAvailableDigits
  where
    permutationsOfAvailableDigits = permutationsOf availableDigits
    reducedNumber = number `mod` permutationsOfAvailableDigits
    foundDigit = availableDigits !! (number `div` permutationsOfAvailableDigits)
    outstandingAvailableDigits = filter (\digit -> digit /= foundDigit) availableDigits

main = return (findLexicographicalPermutation 3 [0..2])
-- findLexicographicalPermutation 1000000, [0..9]
