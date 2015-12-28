module CIS194.Week04.HigherOrderSpec where

import CIS194.Week04.HigherOrder
import Test.Hspec

spec :: Spec
spec = do
  describe "fun1" $
    it "computes a single value for a given list" $ do
      fun1 [10, 3, 1] `shouldBe` 8
      fun1 [7, 4, 5]  `shouldBe` 2

  describe "foldTree" $ do
    it "folds a tree for a given list" $ do
      foldTree ['A']      `shouldBe` Node 0 Leaf 'A' Leaf
      foldTree ['A'..'C'] `shouldBe`
        Node 1 (Node 0 Leaf 'B' Leaf) 'C' (Node 0 Leaf 'A' Leaf)
