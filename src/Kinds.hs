{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Kinds where

import Data.Kind (Constraint, Type)
import Prelude hiding (not)

type MyFunctor :: (Type -> Type) -> Constraint
class MyFunctor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

-- MyFunctor f here has kind Constraint, which is suitable for be on the left side of the fat arrow.
myid :: (MyFunctor f) => f a -> f a
myid x = x

type ReaderT :: Type -> (Type -> Type) -> Type -> Type
newtype ReaderT e m a = ReaderT {runReaderT :: e -> m a}

type MonadTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

not :: Bool -> Bool
not True = False
not False = True

type List :: Type -> Type
data List a = Nil | Cons a (List a)

type Either' :: Type -> Type -> Type
data Either' a b = Left' a | Right' b

class C a b | a -> b where
  op :: a -> b -> ()

-- ghc will not quantify over b for us since it is not *real* polymorphism
foo :: (C Bool b) => b -> ()
foo x = op True x

x :: Int
x = 5
