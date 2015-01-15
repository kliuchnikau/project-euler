-- https://projecteuler.net/problem=9
import Data.List

specialPythagoreanTripletProduct :: Integer
specialPythagoreanTripletProduct = (\(a,b,c) -> a*b*c) specialPythagoreanTriplet
  where
    specialPythagoreanTriplet = head $ filter isPythagoreanTriplet specialTriplets
    specialTriplets = [(a,b,1000-a-b) | a <- [1..1000], b <- [a+1..1000], a + b < 1000]
    isPythagoreanTriplet (a,b,c) = a^2 + b^2 == c^2
