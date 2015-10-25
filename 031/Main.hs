module Main where

expectedSum = 200

allCombinationsOfCoins = [ (ones, twos, fives, tens, twenties, fiftys, hundreds, twohundreds) |
  ones <- [0..expectedSum],
  twos <- [0..(expectedSum-ones) `div` 2],
  fives <- [0..(expectedSum-ones-2*twos) `div` 5],
  tens <- [0..(expectedSum-ones-2*twos-5*fives) `div` 10],
  twenties <- [0..(expectedSum-ones-2*twos-5*fives-10*tens) `div` 20],
  fiftys <- [0..(expectedSum-ones-2*twos-5*fives-10*tens-20*twenties) `div` 50],
  hundreds <- [0..2],
  twohundreds <- [0..1] ]

findCountCombinations = length $ filter sumEqualsExpected allCombinationsOfCoins
  where
    sumEqualsExpected coinsCombination = expectedSum == (sumCoins coinsCombination)
    sumCoins (ones, twos, fives, tens, twenties, fiftys, hundreds, twohundreds) =
      ones*1 + twos*2 + fives*5 + tens*10 + twenties*20 + fiftys*50 + hundreds*100 + twohundreds*200

main = print $ findCountCombinations

{-
---- This is very fast recursive solution
main :: IO ()
main = print $ count [200,100,50,20,10,5,2,1] 200

count :: [Integer] -> Integer -> Integer
count _ 0      = 1
count [c] _    = 1
count (c:cs) s = sum $ map (count cs . (s-)) [0,c..s]

---- One more
coins = [1,2,5,10,20,50,100,200]

sumCoins :: Int -> Int -> Int
sumCoins amount coin
  | amount == 0 = 1
  | coin == 0 = 1
  | otherwise = sum $ map (\x -> sumCoins (amount - (vcoin * x)) (pred coin)) ncoins
  where vcoin = coins !! coin
        ncoins = takeWhile (\k -> (vcoin * k) <= amount) [0..]

main = print $ sumCoins 200 7
-}
