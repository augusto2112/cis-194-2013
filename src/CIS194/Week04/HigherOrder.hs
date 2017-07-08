module CIS194.Week04.HigherOrder where

-- | Exercise 01

fun1 :: [Int] -> Int
fun1 = product . map (subtract 2) . filter even


-- | Exercise 02

type Height = Int

data Tree a = Leaf
            | Node Height (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert val Leaf = Node 0 Leaf val Leaf
insert val (Node ht lt root rt)
  | height lt <= height rt = Node (height left + 1) left root rt
  | otherwise              = Node (height right + 1) lt root right
  where
    left  = insert val lt
    right = insert val rt

    height :: Tree a -> Int
    height Leaf            = -1
    height (Node ht _ _ _) = ht


-- | Exercise 03

xor :: [Bool] -> Bool
xor = foldr xor' False
  where
    xor' a b = (a || b) && not (a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


-- | Exercise 04

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = fmap (\n -> 2 * n + 1) remaining
  where
    bounds    = [1..n]
    remaining = filter (`notElem` removable) bounds
    removable = filter (<= n) [i + j + 2 * i * j | i <- bounds, j <- bounds, i <= j]
