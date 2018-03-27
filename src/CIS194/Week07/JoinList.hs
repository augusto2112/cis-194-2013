{-# OPTIONS_GHC -Wall #-}

import Sized    

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) left right = Append ((tag left) `mappend` (tag right)) left right

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ index (Single m a)
    | index == 0 = Just a
    | otherwise = Nothing
indexJ index (Append m left right)
    | index < half = indexJ index left
    | index >= half = indexJ (index - half) right
    where half = ((getSize . size) m) / 2
