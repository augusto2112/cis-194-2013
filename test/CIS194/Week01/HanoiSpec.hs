module CIS194.Week01.HanoiSpec where

import           CIS194.Week01.Hanoi
import           Test.Hspec

spec :: Spec
spec =
  describe "hanoi" $
    it "solves the Hanoi problem" $
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
