{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module T4 () where

apply :: (forall a. a -> a) -> (Bool, Int)
apply f = (f True, f 3)

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL x = sum (toList x)

data Pair' a b = Pair' a b

-- apply' :: (a -> a) -> (Bool, Int)
-- apply' f = (f True, f 3)
