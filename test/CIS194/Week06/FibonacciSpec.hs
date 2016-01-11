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

  describe "streamToList" $
    it "converts a stream into a list" $
      let stream = streamRepeat 10
      in (take 2 . streamToList $ stream) `shouldBe` [10, 10]

  describe "show" $
    it "conveniently prints the first 20 elements in a stream" $
      let stream = streamRepeat 10
      in show stream `shouldBe` (show [10 | _ <- [1..20]])
