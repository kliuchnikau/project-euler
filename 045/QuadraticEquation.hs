module QuadraticEquation where

data QEquation = QEquation { aVal :: Double, bVal :: Double, cVal :: Double } deriving (Show)

-- find roots of a*x^2 + bx + c = 0
solveEquation :: QEquation -> (Double, Double)
solveEquation (QEquation {aVal=a, bVal=b, cVal=c}) = if discriminant<0 then error "negative discriminant" else (x,y)
  where
    discriminant = b^2 - 4*a*c
    x = (-b + sqrt discriminant) / (2*a)
    y = (-b - sqrt discriminant) / (2*a)
