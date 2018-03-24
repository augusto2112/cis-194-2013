module CIS194.Week06.Fibonacci where

import Data.Map (Map)
import qualified Data.Map as Map

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

dictFunc :: Map Integer Integer -> Integer -> Maybe Integer
dictFunc m i = Map.lookup i m

-- fibs2 :: [Integer]
-- fibs2 = map fib [0..]
--     where Map
