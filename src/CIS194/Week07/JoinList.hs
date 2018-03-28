{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Data.Monoid
import Sized    
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

c :: JoinList Size Char
c = Append (Size 4) 
    (Append (Size 3) 
        (Single (Size 1) 'y') 
        (Append (Size 2) 
            (Single (Size 1) 'e') 
            (Single (Size 1) 'a'))) 
    (Single (Size 1) 'h')

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_) !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) left right = Append ((tag left) <> (tag right)) left right

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ index (Single _ a)
    | index == 0 = Just a
    | otherwise = Nothing
indexJ index (Append _ left right)
    | index < leftSize = indexJ index left
    | otherwise = indexJ (index - leftSize) right
    where leftSize = getSize $ size $ tag left


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n tree | n <= 0 = tree
dropJ _ (Single _ _) = Empty
dropJ n (Append _ left right)
    | n < leftSize = (dropJ n left) +++ right
    | otherwise = (dropJ (n - leftSize) right)
    where leftSize = getSize $ size $ tag left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ tree@(Single _ _) = tree
takeJ n (Append _ left right)
    | n < leftSize = takeJ n left
    | otherwise = left +++ (takeJ (n - leftSize) right)
    where leftSize = getSize $ size $ tag left

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ a) = a
    toString (Append _ left right) = toString left ++ toString right

    fromString s = foldr1 (+++) (map (\x -> Single (scoreString x, (Size 1)) x) (lines s))

    line = indexJ

    replaceLine _ _ Empty = Empty
    replaceLine n s tree
        | n <= treeSize =  takeJ n tree +++ fromString s +++ (takeJ (treeSize - n) (dropJ n tree))
        | otherwise = tree
        where treeSize = getSize $ snd $ tag tree

    numLines = getSize . snd . tag

    value tree = (\(Score x) -> x) $ fst $ tag tree

main :: IO()
main = runEditor editor $ Single ((Score 0), (Size 0)) ""