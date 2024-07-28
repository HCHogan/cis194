{-# LANGUAGE TupleSections #-}

module Arrows where

import Control.Arrow
import qualified Control.Category as Cat
import Control.Monad
import Data.List
import Data.Maybe
import System.Random

-- Arrows, like monads, express computations that happen within a context. However, they are
-- a more general abstraction than monads, and thus allow for contexts beyond what the Monad
-- class makes possible. The essential difference between the abstractions can be summed up thus:
--

-- A plain Haskell function treated as an arrow has type a -> b. Our Circuit arrow has two
-- distinguishing features: First, we wrap it in a newtype declaration to cleanly define
-- an Arrow instance. Second, in order for the circuit to maintain its own internal state,
-- our arrow returns a replacement for itself along with the normal b output value.
newtype Circuit a b = Circuit {unCircuit :: a -> (Circuit a b, b)}

instance Cat.Category Circuit where
  id = Circuit (Cat.id,)
  (.) = dot
   where
    (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
      let (cir1', b) = cir1 a
          (cir2', c) = cir2 b
       in (cir2' `dot` cir1', c)

instance Arrow Circuit where
  arr f = Circuit $ \a -> (arr f, f a)
  first (Circuit cir) = Circuit $ \(b, d) ->
    let (cir', c) = cir b
     in (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit cir (x : xs) =
  let (cir', x') = unCircuit cir x
   in x' : runCircuit cir' xs

runCircuit' :: Circuit a b -> [a] -> [b]
runCircuit' cir inputs =
  snd $ mapAccumL unCircuit cir inputs

accum' :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum' acc f = Circuit $ \input
