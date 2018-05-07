{-# OPTIONS_GHC -Wall #-}

import Data.Char

type Name = String
data Employee = Emp { name :: Name, phone :: String }

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
    where
        f xs
            | null ns   = Nothing
            | otherwise = Just (read ns, rest)
            where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = ((f a), c)

instance Functor Parser where
    fmap g (Parser f) = Parser  (\s -> fmap (first g) (f s)) 

instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    p1 <*> p2 = Parser (\s -> case runParser p1 s of
        Nothing -> Nothing
        Just (a, s1) -> case runParser p2 s1 of
            Nothing -> Nothing
            Just (b, s2) -> Just ((a b), s2))

abParser :: Parser (Char, Char)
abParser = (\a b -> (a, b)) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ =  (\(_, _) -> ()) <$> abParser

spaceParser :: Parser String
spaceParser = pure (dropWhile isSpace)

intPair:: Parser [Integer]
intPair = (\x -> [x]) <$> posInt