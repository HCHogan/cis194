module Hw7.JoinList where

import Hw7.Buffer
import Hw7.Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n _ | n < 0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ x)
  | i == 0 = Just x
  | otherwise = Nothing
indexJ i (Append m l r)
  | i > getSize (size m) = Nothing
  | i < getSize (size $ tag l) = indexJ i l
  | otherwise = indexJ (i - getSize (size $ tag l)) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l
  | n < 0 || n >= getSize (size $ tag l) =
      l
dropJ _ Empty = Empty
dropJ n s@(Single _ _)
  | n == 0 = Empty
  | otherwise = s
dropJ n (Append _ l r)
  | n < getSize (size $ tag l) = dropJ n l +++ r
  | otherwise = l +++ dropJ (n - getSize (size $ tag l)) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Append m l r)
  | n <= 0 || n >= getSize (size m) = Empty
  | n < getSize (size $ tag l) = takeJ n l
  | n == getSize (size $ tag l) = l
  | otherwise = l +++ takeJ (n - getSize (size $ tag l)) r
takeJ _ _ = Empty

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
