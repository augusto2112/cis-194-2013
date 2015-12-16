module CIS194.Week01.LuhnSpec where

import CIS194.Week01.Luhn
import Test.Hspec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts a natural number to a list of digits" $
      toDigits 1234 `shouldBe` [1, 2, 3, 4]

    it "returns an empty list for 0" $
      toDigits 0 `shouldBe` []

    it "returns an empty list for negative numbers" $
      toDigits (-1) `shouldBe` []

  describe "toDigitsRev" $
    it "converts a natural number to a list of digits reversed" $
      toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

  describe "doubleEveryOther" $
    it "doubles every number in a list from right to left" $ do
      doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]

  describe "sumDigits" $
    it "calculate the sum of all digits" $ do
      sumDigits [16,7,12,5] `shouldBe` 22

  describe "validate" $
    it "validates a credit card number" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881882 `shouldBe` False
