{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

-- module CIS194.Week05.Calc where

import ExprT
import Parser
import qualified StackVM as S

eval :: ExprT -> Integer
eval (Lit val) = val
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)

evalStr :: String -> Maybe Integer
evalStr s =
  case parseExp Lit Add Mul s of
    Nothing -> Nothing
    (Just expr) -> Just $ eval expr

class Expr exp where
  lit :: Integer -> exp
  add :: exp -> exp -> exp
  mul :: exp -> exp -> exp

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit exp = exp
  add exp1 exp2 = exp1 + exp2
  mul exp1 exp2 = exp1 * exp2

instance Expr Bool where
  lit exp = exp > 0
  add exp1 exp2 = exp1 || exp2
  mul exp1 exp2 = exp1 && exp2

newtype MinMax = MinMax Integer
                deriving (Show, Eq)

instance Expr MinMax where
  lit exp = MinMax exp
  add (MinMax exp1) (MinMax exp2) = MinMax $ max exp1 exp2
  mul (MinMax exp1) (MinMax exp2) = MinMax $ min exp1 exp2

newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr Mod7 where
  lit exp = Mod7 $ exp `mod` 7
  add (Mod7 exp1) (Mod7 exp2) = lit $ exp1 + exp1
  mul (Mod7 exp1) (Mod7 exp2) = lit $ exp1 * exp1

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "3 + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr S.Program where
  lit num = (S.PushI num) : []
  add exp1 exp2 = exp1 ++ exp2 ++ [S.Add]
  mul exp1 exp2 = exp1 ++ exp2 ++ [S.Mul]


compile :: String -> Maybe S.Program
compile = parseExp lit add mul

b = compile "3 + 4 * 5"

exec :: Maybe S.Program -> Either String S.StackVal
exec Nothing = Left "Nothing"
exec (Just b) = S.stackVM b





























