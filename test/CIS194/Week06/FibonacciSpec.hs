module CIS194.Week06.FibonacciSpec where

import CIS194.Week06.Fibonacci
import Test.Hspec

spec :: Spec
spec = do
  describe "fibs1" $ do
    it "computes the 10 first fibonacci numbers" $
      take 10 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
