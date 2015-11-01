module Main where

allFractions = [ (nom, denom) | nom <- [10..99], denom <- [10..99], not (nom == denom)]

cancellingFraction (nom, denom) = can_be_cancelled && cancellation_produces_valid_fraction
  where
    can_be_cancelled = (nom `mod` 10) == (denom `div` 10)
    cancellation_produces_valid_fraction = (nom * reduced_denom) == (denom * reduced_nom)
    cancelled_nom = nom `div` 10
    cancelled_denom = denom `mod` 10
    cancelled_reductor = gcd cancelled_nom cancelled_denom
    reduced_nom = cancelled_nom `div` cancelled_reductor
    reduced_denom = cancelled_denom `div` cancelled_reductor

productTerms terms = (productNom `div` theirGcd, productDenom `div` theirGcd)
  where
    productNom = product $ map fst terms
    productDenom = product $ map snd terms
    theirGcd = gcd productNom productDenom

allCancellingFractions = snd $ productTerms $ filter cancellingFraction allFractions

main = print $ allCancellingFractions
