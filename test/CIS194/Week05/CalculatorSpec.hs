module CIS194.Week05.CalculatorSpec where

import CIS194.Week05.Calculator
import CIS194.Week05.ExprT
import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $ do
    it "computes the given expression" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
