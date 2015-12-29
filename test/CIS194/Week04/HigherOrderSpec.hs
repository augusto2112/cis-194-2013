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

  describe "xor" $
    it "applies xor to a given list of boolean values" $ do
      xor [False, True, False]              `shouldBe` True
      xor [False, True, False, False, True] `shouldBe` False

  describe "map'" $
    it "applies a function to every element in a given list" $ do
      map' (*2) [1, 2, 3]         `shouldBe` [2, 4, 6]
      map' (subtract 2) [1, 2, 3] `shouldBe` [-1, 0, 1]

  describe "sieveSundaram" $
    it "finds all primes up to a specified integer" $ do
      sieveSundaram 5  `shouldBe` [3,5,7,11]
      sieveSundaram 10 `shouldBe` [3,5,7,11,13,17,19]
