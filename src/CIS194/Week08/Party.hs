{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + (empFun e))

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL a1 f1) (GL a2 f2) = GL (a1 <> a2) (f1 + f2) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a cs) = f a (map (treeFold f) cs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss (mconcat withoutSubBoss), (mconcat (map (uncurry moreFun) gls)))
    where (_, withoutSubBoss) = unzip gls

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel 

printFun :: GuestList -> String
printFun (GL es fs) = "Total fun: " ++ show fs ++ "\n" ++ unlines (map empName es)

main :: IO ()
main = (readFile "company.txt" >>= (\c -> putStr (printFun (maxFun (read c)))))


-- instance Functor ((,) e) where
--     fmap f (x,y) = (x, (f y))

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

data ITree a = ILeaf (Int -> a) 
             | INode [ITree a]

instance Functor ITree where
    fmap f (ILeaf g) = ILeaf (f . g)
    fmap f (INode xs) = INode (fmap (fmap f) xs)