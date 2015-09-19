module SolutionSpec where

import Test.Hspec
import Solution

main :: IO ()
main = hspec $ do
  describe "primesStreakLength" $ do
    specify "known example 1 (Euler formula)" $
      primesStreakLength (1,41) `shouldBe` 40

    specify "known example 2" $
      primesStreakLength (-79,1601) `shouldBe` 80
