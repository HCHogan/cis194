module Travel where
import Control.Applicative (Const(getConst, Const))

-- We already have studied four of the five type classes in the Prelude
-- that can be used for data structure manipulation: Functor, Applicative,
-- Monad and Foldable. The fifth one is Traversable [1]. To traverse means to walk across,
-- and that is exactly what Traversable generalises: walking across a structure, collecting results
-- at each stop.

-- foldMap and Functor are not enough to express all userful ways of traversing.
-- Fortunately, there is a type class which is essentially about combining Functor contexts: Applicative
-- Traversable is to Applicative contexts what Foldable is to Monoid values.
-- From that point of view, sequenceA is analogous to fold − it creates an applicative summary of
-- the contexts within a structure, and then rebuilds the structure in the new context.

-- In general, it is better to write traverse when implementing Traversable, as the default definition
-- of traverse performs, in principle, two runs across the structure (one for fmap and another for
-- sequenceA).

-- Sensible instances of Traversable have a set of laws to follow. There are the following two laws:
-- traverse Identity = Identity -- identity
-- traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f -- composition

-- If t is an applicative homomorphism, then
-- t . traverse f = traverse (t . f) -- naturality

testIO :: (a -> IO b) -> [a] -> IO [b]
testIO = traverse

foldMap' :: (Traversable t, Monoid c) => (a -> c) -> t a -> c
foldMap' f = getConst . traverse (Const . f)
-- from Applicative Const we can see clearly that applicative is indeed just a combination of contexts.
