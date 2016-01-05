module CIS194.Week06.Fibonacci where

-- Exercise 01
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- The implementation was borrowed from:
-- https://wiki.haskell.org/The_Fibonacci_sequence
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
