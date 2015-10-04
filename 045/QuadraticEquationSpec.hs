module QuadraticEquationSpec where

import Test.Hspec
import QuadraticEquation

main :: IO ()
main = hspec $ do
  describe "solveEquation" $ do
    context "when equation has non-complex solution" $ do
      it "returns two roots of equation" $ do
        solveEquation (QEquation 1.0 5.0 6.0) `shouldBe` (-2.0, -3.0)
