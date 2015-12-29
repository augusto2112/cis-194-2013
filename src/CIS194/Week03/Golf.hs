module CIS194.Week03.Golf where

import Data.Bool

-- Exercise 01
skips :: [a] -> [[a]]
skips xs = map nth [1..(length xs)]
  where
    nth n = map snd $ filter (\(i, _) -> i `mod` n == 0) $ zip [1..] xs

-- Exercise 02
localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

-- Exercise 03
histogram :: [Int] -> String
histogram xs = (unlines . lines') xs ++ "==========\n0123456789\n"

lines' :: [Int] -> [String]
lines' xs = map (line freq) boundaries
  where
    freq       = frecuency xs
    max        = maximum freq
    boundaries = [max, (max - 1)..1]

line :: [Int] -> Int -> String
line xs n = map (bool ' ' '*' . (n <=)) xs

frecuency :: [Int] -> [Int]
frecuency xs = map count [0..9]
  where
    count n = length $ filter (== n) xs
