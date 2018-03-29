{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL employees fun) = GL (emp:employees) (fun + empFun emp)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 <> emps2)  (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ fun1) b@ (GL _ fun2) = if fun1 >= fun2 then a else b

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (glCons boss mempty, mempty)
nextLevel boss ((withSubBoss, withoutSubBoss):[]) = (glCons boss withoutSubBoss, withSubBoss)
nextLevel boss ((withSubBoss, withoutSubBoss):xs) = let (withBoss, withoutBoss) =  nextLevel boss xs in
                        (withoutSubBoss <> withBoss,  withSubBoss <> withoutBoss)

maxFun :: Tree Employee -> GuestList
maxFun t = let (list1, list2) = (treeFold nextLevel t) in moreFun list1 list2

readGuestList :: String -> Tree Employee
readGuestList = read

formatGuestList :: GuestList -> String
formatGuestList (GL emps fun) = "Total fun: " ++ show fun ++ "\n" ++ (foldr (\emp x -> x ++ (empName emp) ++ "\n") "" emps)

main :: IO()
main = readFile "company.txt" >>= (\f -> return $ formatGuestList $ maxFun $ readGuestList f) >>= putStr