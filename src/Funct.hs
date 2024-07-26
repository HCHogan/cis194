module Funct where

data Tree a = Node a [Tree a]

instance Functor Tree where
  fmap f (Node x ls) = Node (f x) (map (fmap f) ls)

-- instance Functor ((->) e) where
--   fmap = (.)

--  The existence of two legal implementations of (<*>) for lists which only differ in the 
--  sequencing of effects indicates that [] is a non-commutative applicative functor. A 
--  commutative applicative functor, by contrast, leaves no margin for ambiguity in that 
--  respect. More formally, a commutative applicative functor is one for which the following holds:
-- ```haskell 
-- liftA2 f u v = liftA2 (flip f) v u -- Commutativity
-- ```
-- Or, equivalently,
-- ```haskell
-- f <$> u <*> v = flip f <$> v <*> u
-- ```

-- The convention in Haskell is to always implement (<*>) and other applicative operators using 
-- left-to-right sequencing. Even though this convention helps reducing confusion, it also 
-- means appearances sometimes are misleading. For instance, the (<*) function is not flip (*>), 
-- as it sequences effects from left to right just like (*>)
--
-- To invert the sequencing of effects, we can use the `Control.Applicative.Backwards` from transformers
-- >>> Backwards [(2*), (3*)] <*> Backwards [4, 5] = Backwards [8, 12, 10, 15]


