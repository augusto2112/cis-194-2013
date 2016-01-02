module CIS194.Week05.CalculatorSpec where

import CIS194.Week05.Calculator
import CIS194.Week05.ExprT
import CIS194.Week05.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $
    it "computes the given expression" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $
    it "evaluates a given string" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20
      evalStr "(2+3)*"  `shouldBe` Nothing

  describe "Expr" $ do
    it "evaluates an expression" $
      (reify $ mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe`
        Mul (Add (Lit 2) (Lit 3)) (Lit 4)

    it "works with integers" $ do
      testInteger "(3 * -4) + 5" `shouldBe` Just (-7)
      testInteger "(3 * 4) + 5"  `shouldBe` Just 17

    it "works with booleans" $ do
      testBool "(3 * -4) + 5"  `shouldBe` Just True
      testBool "(3 * -4) + -5" `shouldBe` Just False

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

testInteger :: String -> Maybe Integer
testInteger = testExp

testBool :: String -> Maybe Bool
testBool = testExp
