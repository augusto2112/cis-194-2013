module CIS194.Week03.GolfSpec where

import CIS194.Week03.Golf
import Test.Hspec

spec :: Spec
spec = do
  describe "skips" $ do
    it "convert a list into a list of lists" $ do
      skips "ABCD"   `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1]      `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
