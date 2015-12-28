module CIS194.Week04.HigherOrder where

-- Exercise 01
fun1 :: [Int] -> Int
fun1 = product . map (\x -> x - 2) . filter even
