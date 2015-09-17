module SolutionSpec where

import Test.Hspec
import Solution

main :: IO ()
main = hspec $ do
  -- https://en.wikipedia.org/wiki/Primitive_root_modulo_n
  describe "isPrimitiveRootModulo" $ do
    specify "True when all numbers from 1 to p-1 can be represented as p^k mod 10, where k=0..p-2" $
      (10 `isPrimitiveRootModulo` 7) `shouldBe` True

    specify "False otherwise" $
      (10 `isPrimitiveRootModulo` 11) `shouldBe` False
