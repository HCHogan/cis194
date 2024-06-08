{-# LANGUAGE DeriveFunctor #-}

module T11 (employees1, employees2) where

type Name = String

data Employee = Employee
  { name :: Name
  , phone :: String
  }
  deriving (Show)

names :: [String]
names = ["Joe", "Sara", "Mae"]

phones :: [String]
phones = ["555-5555", "123-456-7890", "555-4321"]

employees1 :: [Employee]
employees1 = Employee <$> names <*> phones

newtype FuckList a = FuckList {getFuckList :: [a]}
  deriving (Eq, Show, Functor)

instance Applicative FuckList where
  pure = FuckList . repeat
  FuckList fs <*> FuckList xs = FuckList (zipWith ($) fs xs)

employees2 :: [Employee]
employees2 = getFuckList $ Employee <$> FuckList names <*> FuckList phones


