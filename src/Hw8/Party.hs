{-# OPTIONS_GHC -Wno-orphans #-}

module Hw8.Party (glCons, moreFun, treeFold, nextLevel, hw8Main) where

import Control.Monad (forM_)
import Data.Tree
import Hw8.Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

instance Semigroup GuestList where
  (GL es1 fun1) <> (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob xs = (glCons bob $ mconcat $ map snd xs, mconcat $ map (uncurry moreFun) xs)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

hw8Main :: IO ()
hw8Main = do
  company <- read <$> readFile "src/Hw8/company.txt"
  let (GL guests fun) = maxFun company
  putStrLn $ "Total fun: " ++ show fun
  forM_ guests (putStrLn . empName)

{-
 - we will compute two guest lists:
 - 1. the best possible guest list we can create if we invite the boss (that
 - is, the Employee at the root of the tree); and
 - 2. the best possible guest list we can create if we don’t invite the boss.
 - It turns out that this gives us enough information at each step to
 - compute the optimal two guest lists for the next level up.
 -
 - The first is the “boss” of the current subtree (let’s call him Bob).
 - The second argument is a list of the results for each subtree under Bob
 - Each result is a pair of GuestLists: the
 - first GuestList in the pair is the best possible guest list with the boss
 - of that subtree; the second is the best possible guest list without the
 - boss of that subtree. nextLevel should then compute the overall best
 - guest list that includes Bob, and the overall best guest list that doesn’t
 - include Bob.
 -}
