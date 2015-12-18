module CIS194.Week02.LogAnalysisSpec where

import CIS194.Week02.LogAnalysis
import CIS194.Week02.Log
import Test.Hspec

spec :: Spec
spec =
  describe "parseMessage" $ do
    it "parses an info message" $
      parseMessage "I 4 Everything normal" `shouldBe` LogMessage Info 4 "Everything normal"
    it "parses a warning message" $
      parseMessage "W 5 Flange is due for a check-up" `shouldBe` LogMessage Warning 5 "Flange is due for a check-up"
    it "parses an error message" $
      parseMessage "E 70 3 Way too many pickles" `shouldBe` LogMessage (Error 70) 3 "Way too many pickles"
    it "parses unknown message" $
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

