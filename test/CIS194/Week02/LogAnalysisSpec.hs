module CIS194.Week02.LogAnalysisSpec where

import CIS194.Week02.LogAnalysis
import CIS194.Week02.Log
import Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "parses an info message" $
      parseMessage "I 4 Everything normal" `shouldBe` LogMessage Info 4 "Everything normal"
    it "parses a warning message" $
      parseMessage "W 5 Flange is due for a check-up" `shouldBe` LogMessage Warning 5 "Flange is due for a check-up"
    it "parses an error message" $
      parseMessage "E 70 3 Way too many pickles" `shouldBe` LogMessage (Error 70) 3 "Way too many pickles"
    it "parses unknown message" $
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

  describe "parse" $ do
    it "parses an info message" $
      parse "I 4 Everything normal\nW 5 Flange is due for a check-up" `shouldBe`
      [ LogMessage Info 4 "Everything normal"
      , LogMessage Warning 5 "Flange is due for a check-up"
      ]

  describe "insert" $ do
    context "when the LogMessage is Unknown" $ do
      context "and the target is a Leaf" $
        it "do not change the MessageTree" $
          insert (Unknown "") Leaf `shouldBe` Leaf
      context "and the target is a Node" $
        it "do not change the MessageTree" $
          insert (Unknown "") (messageTree 10) `shouldBe` (messageTree 10)

    context "when the target is a Leaf" $
      it "inserts the given LogMessage into the MessageTree" $
        insert (logMessage 10) Leaf `shouldBe` (messageTree 10)

    context "when the log message has a timestamp smaller or equal than root" $
      it "inserts the given LogMessage to the left" $ do
        insert (logMessage 1)  (messageTree 10) `shouldBe` Node (messageTree 1)  (logMessage 10) Leaf
        insert (logMessage 10) (messageTree 10) `shouldBe` Node (messageTree 10) (logMessage 10) Leaf

    context "when the log message has a timestamp greater than root" $
      it "inserts the given LogMessage to the right" $
        insert (logMessage 10) (messageTree 1) `shouldBe` Node Leaf (logMessage 1) (messageTree 10)

  describe "build" $ do
    it "builds up a MessageTree from a list of log messages" $
      build [ logMessage 1, logMessage 10 ] `shouldBe` Node Leaf (LogMessage Info 1 "") (Node Leaf (LogMessage Info 10 "") Leaf)

  describe "inOrder" $ do
    it "sorts log messages by timestamp" $
      inOrder (Node (messageTree 1) (logMessage 2) (messageTree 3)) `shouldBe` [ (logMessage 1), (logMessage 2), (logMessage 3)]

-- Helper functions
logMessage :: TimeStamp -> LogMessage
logMessage ts = LogMessage Info ts ""
logMessage ts = LogMessage Info ts ""

messageTree :: TimeStamp -> MessageTree
messageTree ts = Node Leaf (logMessage ts) Leaf

