module QuadraticEquation where

import Data.List

data QEquation = QEquation { aVal :: Double, bVal :: Double, cVal :: Double } deriving (Show)

-- when discriminant < 0 we get complex numbers. for now let's ignore complex roots
equationRoots :: QEquation -> [Double]
equationRoots (QEquation {aVal=a, bVal=b, cVal=c}) = if discriminant<0 then [] else nub [x,y]
  where
    discriminant = b^2 - 4*a*c
    x = (-b + sqrt discriminant) / (2*a)
    y = (-b - sqrt discriminant) / (2*a)

substituteUnknown :: QEquation -> Double -> Double
substituteUnknown (QEquation {aVal=a, bVal=b, cVal=c}) x = a*x^2 + b*x + c

naturalRoots :: QEquation -> [Integer]
naturalRoots eq = filter stillEquationRoot $ toNaturalNumbers $ equationRoots eq
  where
    toNaturalNumbers :: [Double] -> [Integer]
    toNaturalNumbers xs = filter (>= 0) $ map (toInteger . round) xs
    stillEquationRoot :: Integer -> Bool
    stillEquationRoot x = (substituteUnknown eq value) == 0.0
      where
        value = fromIntegral x :: Double
