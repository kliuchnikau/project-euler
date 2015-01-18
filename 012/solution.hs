-- https://projecteuler.net/problem=12
import Factors
import Primes

triangles :: [Int]
triangles = 1:[ (idx+1) + triangle(idx-1) | idx <- [1..]]

triangle :: Int -> Int
triangle num = triangles !! num

primeFactors :: Int -> [Int]
primeFactors num = filter (\c -> num `mod` c == 0) candidates
  where
    candidates = takeWhile (\p -> p*p <= num) primes

factorMultiplicity :: Int -> Int -> Int
factorMultiplicity base factor | factor == 1 = base `div` 1
                               | (base `mod` factor) == 0 = 1 + factorMultiplicity (base `div` factor) factor
                               | otherwise = 0

nFactors :: Int -> Int
nFactors num = product (map (\factor -> factorMultiplicity num factor + 1) (primeFactors num))

highlyDivisibleTriangularNum :: Int -> Int
highlyDivisibleTriangularNum minFactors = head (filter withNfactors triangles)
  where
    withNfactors triangleNum = nFactors(triangleNum) >= minFactors
