module CIS194.Week05.Calculator where

import CIS194.Week05.ExprT

-- Exercise 01
eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add lt rt) = eval lt + eval rt
eval (Mul lt rt) = eval lt * eval rt
