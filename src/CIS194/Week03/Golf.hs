{-# OPTIONS_GHC -Wall #-}

module CIS194.Week03.Golf where

nthElements :: Int -> [a] -> [a]
nthElements n xs = case drop (n-1) xs of
  [] -> []
  (y:ys) -> y : nthElements n ys

skips :: [a] -> [[a]]
skips xs =  map (flip nthElements xs) [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

count :: Integer -> [Integer] -> Integer
count n = toInteger . length . filter (==n)

countAll :: [Integer] -> [Integer]
countAll xs = map (flip count xs) [0..9]

histogramLine :: [Integer] -> String
histogramLine = map (\x -> if x > 0 then '*' else ' ')

subtractValue :: Integer -> [Integer] -> [Integer]
subtractValue n = map (\x -> max(x - n) 0)

isZero :: [Integer] -> Bool
isZero = all (==0)

makeLines :: [Integer] -> [String]
makeLines xs
  | isZero xs = []
  | otherwise = [(histogramLine xs)] ++ (makeLines (subtractValue 1 xs))

histogram :: [Integer] -> String
histogram xs = unlines ((reverse . makeLines . countAll) xs ++ ["==========", "0123456789"])

