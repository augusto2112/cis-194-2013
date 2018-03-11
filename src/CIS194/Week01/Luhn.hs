{-# OPTIONS_GHC -Wall #-}
module CIS194.Week01.Luhn where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | n < 10    = [n]
    | otherwise = n `mod` 10 : toDigitsRev (n `quot` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherInOrder :: [Integer] -> [Integer]
doubleEveryOtherInOrder []          = []
doubleEveryOtherInOrder [x]         = [x]
doubleEveryOtherInOrder (x:(y:zs))  = x : (y*2) : doubleEveryOtherInOrder zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherInOrder . reverse

sumNumber  :: Integer -> Integer
sumNumber = sum . toDigits

sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumNumber

validate :: Integer -> Bool
validate n = ((sumDigits . doubleEveryOther . toDigits) n) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)



