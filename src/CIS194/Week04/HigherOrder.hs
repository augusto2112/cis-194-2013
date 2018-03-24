{-# OPTIONS_GHC -Wall #-}

module CIS194.Week04.HigherOrder where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- fun2' :: Integer -> Integer
-- fun2' n = takeWhile (>1) $ iterate (\x -> if even x  then x + (x `div` 2) else fun2' (3 * x + 1) ) n

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insertNode :: a -> Tree a -> Tree a
insertNode val Leaf  = (Node 0 Leaf val Leaf)
insertNode val (Node level Leaf a Leaf)  = (Node (level+1) (insertNode val Leaf ) a Leaf)
insertNode val (Node level Leaf a right) = (Node level (insertNode val Leaf ) a right)
insertNode val (Node level left a Leaf)  = (Node level left a (insertNode val Leaf))
insertNode val (Node level left a right)
  | level_left <= level_right = Node (level_left+1) new_left a right
  | otherwise                 = Node (level_right+1) left a new_right
  where new_left@(Node level_left _ _ _)  = (insertNode val left)
        new_right@(Node level_right _ _ _) = (insertNode val right)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

xor :: [Bool] -> Bool
xor = foldr (\x y -> if y then not x else x) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]







