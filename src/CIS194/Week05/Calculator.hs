module CIS194.Week05.Calculator where

import CIS194.Week05.ExprT
import CIS194.Week05.Parser

-- Exercise 01
eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add lt rt) = eval lt + eval rt
eval (Mul lt rt) = eval lt * eval rt

-- Exercise 02
evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
                   Just exprT -> Just (eval exprT)
                   Nothing    -> Nothing

-- Exercise 03
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 04
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show, Eq)

instance Expr MinMax where
  lit = MinMax
  add (MinMax lt) (MinMax rt) = MinMax (max lt rt)
  mul (MinMax lt) (MinMax rt) = MinMax (min lt rt)

newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 lt) (Mod7 rt) = Mod7 ((lt + rt) `mod` 7)
  mul (Mod7 lt) (Mod7 rt) = Mod7 ((lt * rt) `mod` 7)
