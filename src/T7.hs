module T7 () where

newtype Sum a = Sum a
  deriving (Eq, Ord, Show)

instance (Num a) => Semigroup (Sum a) where
  (Sum x) <> (Sum y) = Sum (x + y)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

newtype Product a = Product a
  deriving (Eq, Ord, Show)

instance (Num a) => Semigroup (Product a) where
  (Product x) <> (Product y) = Product (x * y)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

newtype B1 = B1 Bool
  deriving (Eq, Ord, Show)

{-
  The Monoid instance for B1 is a bit tricky. The Monoid laws require that mempty be the identity element for the <> operation.
  In this case, the <> operation is the logical OR operation.
  The identity element for the OR operation is False, so we define mempty = B1 False.-}
instance Semigroup B1 where
  (B1 x) <> (B1 y) = B1 (x || y)

instance Monoid B1 where
  mempty = B1 False

newtype B2 = B2 Bool

instance Semigroup B2 where
  (B2 x) <> (B2 y) = B2 (x && y)

instance Monoid B2 where
  mempty = B2 True
