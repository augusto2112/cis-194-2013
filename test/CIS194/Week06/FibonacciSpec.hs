module CIS194.Week06.FibonacciSpec where

import CIS194.Week06.Fibonacci
import Test.Hspec

spec :: Spec
spec = do
  describe "fibs1" $
    it "computes the 10 first fibonacci numbers" $
      take 10 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

  describe "fibs2" $
    it "computes the nth fibonacci number" $ do
      fibs2 !! 100 `shouldBe` 354224848179261915075
      fibs2 !! 150 `shouldBe` 9969216677189303386214405760200
