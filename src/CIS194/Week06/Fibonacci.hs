module CIS194.Week06.Fibonacci where

-- Exercise 01
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 02
-- The implementation was borrowed from:
-- https://wiki.haskell.org/The_Fibonacci_sequence
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 03
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 04
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 05
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = ruler' 0
  where
    ruler' x = interleave (streamRepeat x) (ruler' (x + 1))

interleave :: Stream a -> Stream a -> Stream a
interleave (Cons x xs) ys = Cons x (interleave ys xs)
