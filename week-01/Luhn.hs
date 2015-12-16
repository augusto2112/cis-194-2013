{-# OPTIONS_GHC -Wall #-}

module Luhn where

-- Exercise 01
toDigits :: Int -> [Int]
toDigits = reverse . toDigitsRev

toDigitsRev :: Int -> [Int]
toDigitsRev n
  | n <= 0    = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

-- Exercise 02
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther (x:y:xs) = x : 2*y : doubleEveryOther xs
doubleEveryOther xs       = xs

-- Exercise 03
sumDigits :: [Int] -> Int
sumDigits = sum . map (sum . toDigits)

-- Exercise 04
validate :: Int -> Bool
validate n = mod (sumDigits . doubleEveryOther . toDigitsRev $ n) 10 == 0
