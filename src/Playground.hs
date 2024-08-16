module Playground () where

deleteIfNegative :: (Num a, Ord a) => a -> Maybe a
deleteIfNegative rrr = if rrr < 0 then Nothing else Just rrr

rejectWithNegatives :: (Num a, Ord a) => [a] -> Maybe [a]
rejectWithNegatives = traverse deleteIfNegative

data MyMaybe a = MyNothing | MyJust a deriving (Show)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust v) = MyJust (f v)

instance Applicative MyMaybe where
  pure = MyJust
  MyNothing <*> _ = MyNothing
  _ <*> MyNothing = MyNothing
  (MyJust f) <*> (MyJust v) = MyJust (f v)

instance Monad MyMaybe where
  return = pure
  MyNothing >>= _ = MyNothing
  (MyJust v) >>= f = f v

data MyList a = MyEmpty | MyCons a (MyList a)

instance Functor MyList where
  fmap _ MyEmpty = MyEmpty
  fmap f (MyCons x xs) = MyCons (f x) (fmap f xs)

instance Foldable MyList where
  foldMap _ MyEmpty = mempty
  foldMap f (MyCons x xs) = f x `mappend` foldMap f xs

instance Traversable MyList where
  traverse _ MyEmpty = pure MyEmpty
  traverse f (MyCons x xs) = MyCons <$> f x <*> traverse f xs

data Tree a = Leaf a | Branch a (Tree a) (Tree a)

instance Functor Tree where
  fmap f (Leaf val) = Leaf $ f val
  fmap f (Branch val l r) = Branch (f val) (fmap f l) (fmap f r)

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Branch a l r) = f a <> foldMap f l <> foldMap f r

-- Traversable is to Applicative contexts what Foldable is to Monoid values. From that point of view, sequenceA is analogous to fold − it creates an applicative summary of the contexts within a structure, and then rebuilds the structure in the new context. sequenceA is the function we were looking for:
instance Traversable Tree where
  traverse f (Leaf val) = Leaf <$> f val
  traverse f (Branch val l r) = Branch <$> f val <*> traverse f l <*> traverse f r

_ = traverse (\x -> [0 .. x]) [0 .. 2]
