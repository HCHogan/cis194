module Travel where

-- We already have studied four of the five type classes in the Prelude 
-- that can be used for data structure manipulation: Functor, Applicative, 
-- Monad and Foldable. The fifth one is Traversable [1]. To traverse means to walk across, 
-- and that is exactly what Traversable generalises: walking across a structure, collecting results 
-- at each stop.

-- foldMap and Functor are not enough to express all userful ways of traversing.
