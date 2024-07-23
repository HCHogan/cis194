module Folds where

{-
 - The Foldable type class provides a generalisation of list folding (foldr and friends) and operations derived from it to arbitrary data structures. Besides being extremely useful, Foldable is a great example of how monoids can help formulating good abstractions.
 - -}

import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Functor.Const (Const (..))
import qualified Data.Map as M
import Data.Monoid (Sum (Sum, getSum))

-- import Data.Monoid (Sum(Sum))

foldMap' :: (Monoid m) => (a -> m) -> [a] -> m
foldMap' g = mconcat . fmap g

-- _ = foldMap Sum [1..10]

newtype Endo b = Endo {appEndo :: b -> b}

instance Semigroup (Endo a) where
  Endo g <> Endo f = Endo (g . f)

instance Monoid (Endo a) where
  mempty = Endo id

foldComposing :: (a -> (b -> b)) -> [a] -> Endo b
foldComposing f = foldMap (Endo . f)

myFoldr :: (a -> (b -> b)) -> b -> [a] -> b
myFoldr f z xs = appEndo (foldComposing f xs) z

-- foldMap is the conceptual heart of Foldable, the class which generalizes `foldr` to arbitrary data structures.

testfun :: IO ()
testfun = do
  let testMap = M.fromList $ zip [0 ..] ["Yesterday", "I", "went", "to", "the", "store"]
  print $ length testMap
  print $ sum . fmap length $ testMap
  print $ elem "I" testMap
  traverse_ putStrLn testMap

product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

concat :: (Foldable t) => t [a] -> [a]
concat = foldMap id

concatMap :: (Foldable t) => (a -> [b]) -> t a -> [b]
concatMap = foldMap

all :: (Foldable t) => (a -> Bool) -> t a -> Bool
all f = foldr (\x acc -> f x && acc) True

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr (\x acc -> x == e || acc) False

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (const $ Sum 1)

traverse_' :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_' f = foldr (\x _ -> f x $> ()) (pure ())

mapM_' :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_' f = foldr ((>>) . f) (return ())

safeMaximum' :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum' = foldr (liftA2 max . Just) Nothing

find' :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find' f = foldr (\x acc -> if f x then Just x else acc) Nothing

composeL :: (Foldable t) => (b -> a -> b) -> t a -> b -> b
composeL f ls s = foldr (flip f) s ls

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldMap (: [])

-- toList reflects the fact that lists are the free monoid for Haskell types.
-- "Free" here means any value can be promoted to the monoid in a way which neither adds
-- nor erases any information (we can convert values of type a to [a] lists with a single element and
-- back through (\x->[x]) and head in a lossless way)

data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Branch l r) = foldMap f l <> foldMap f r

-- instance (Monoid a, Monoid b) => Monoid (a, b) where

-- treeDepth :: Tree a -> Int
-- treeDepth

