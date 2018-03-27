module CIS194.Week06.Fibonacci where

import Data.Map (Map)
import qualified Data.Map as Map

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

dictFib :: Map Integer Integer -> Integer -> Map Integer Integer
dictFib m i = case Map.lookup i m of
    Nothing -> let newDict = dictFib (dictFib m (i-1)) (i-2)
            in case ((Map.lookup (i-1) newDict), (Map.lookup (i-2) newDict)) of
            ((Just a), (Just b)) -> (Map.insert i (a+b) newDict) 
            otherwise -> error "Something went wrong"
    Just _ -> m 


search :: Integer -> Integer
search n = case Map.lookup n (dictFib (Map.fromList [(0, 0), (1, 1)]) n) of
    Just a -> a
    otherwise -> error "Something went wrong"

fibs2 :: [Integer]
fibs2 = map search [0..]

fibs3 :: [Integer]
fibs3  = lookInDict (Map.fromList [(0, 0), (1, 1)]) 0
    where lookInDict m i = case Map.lookup i dict of 
            Just a -> a : (lookInDict dict (i+1)) 
            otherwise -> error "Something went wrong"
            where dict = dictFib m i

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . (take 20) . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = (Cons x (Cons y (interleaveStreams xs ys)))

ruler :: Stream Integer
ruler = streamMap (\x -> 2 * x ^ 2) 