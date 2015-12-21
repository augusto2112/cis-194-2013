module CIS194.Week03.Golf where

-- Exercise 01
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (flip nth xs) [1..(length xs)]

nth :: Int -> [a] -> [a]
nth _ [] = []
nth 1 xs = xs
nth n xs = take 1 (drop (n - 1) xs) ++ nth n (drop n xs)
