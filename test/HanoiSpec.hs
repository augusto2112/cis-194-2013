module HanoiSpec where

import Hanoi
import Test.Hspec

spec :: Spec
spec =
  describe "hanoi" $
    it "..." $
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
