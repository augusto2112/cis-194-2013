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

