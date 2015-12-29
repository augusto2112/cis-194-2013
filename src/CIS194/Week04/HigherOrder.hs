module CIS194.Week04.HigherOrder where

-- Exercise 01
fun1 :: [Int] -> Int
fun1 = product . map (\x -> x - 2) . filter even

type Height = Int

data Tree a = Leaf
            | Node Height (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert val Leaf = Node 0 Leaf val Leaf
insert val (Node ht lt root rt)
  | height lt <= height rt =
  let left = insert val lt
  in Node (height left + 1) left root rt
  | otherwise =
  let right = insert val rt
  in Node (height right + 1) lt root right

height :: Tree a -> Int
height Leaf = -1
height (Node ht _ _ _) = ht

xor :: [Bool] -> Bool
xor = foldr xor' False
  where
    xor' :: Bool -> Bool -> Bool
    xor' a b = (a || b) && not (a && b)
