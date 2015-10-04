module Lists where

windowed :: Int -> [Int] -> [[Int]]
windowed size list = map sublist [0..(length(list) - size)]
  where
    sublist offset = take size . drop offset $ list
