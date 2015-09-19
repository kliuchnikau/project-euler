module SolutionSpec where

import Test.Hspec
import Solution

main :: IO ()
main = hspec $ do
  describe "primesStreakLength" $ do
    specify "known example 1 (Euler formula)" $ do
      primesStreakLength (1,41) `shouldBe` 40

    specify "known example 2" $ do
      primesStreakLength (-79,1601) `shouldBe` 80

    context "when produces not prime number straight away" $ do
      it "returns 0" $ do
        primesStreakLength (-10,4) `shouldBe` 0
