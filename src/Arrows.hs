{-# LANGUAGE Arrows #-}
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

addA :: (Arrow a) => a b Int -> a b Int -> a b Int
addA f g = proc x -> do
  y <- f -< x
  z <- g -< x
  returnA -< y + z

-- proc (arrow abstraction) is a kind of lambda, except that it constructs an arrow instead of a function.
-- -< (arrow application) feeds the value of an expression into an arrow.
-- Above is equivalent to:
addA' :: (Arrow cat, Num c) => cat a c -> cat a c -> cat a c
addA' f g =
  arr (\x -> (x, x))
    >>> first f
    >>> arr (\(y, x) -> (x, y))
    >>> first g
    >>> arr (uncurry (+))

-- is equivalent to:
addA'' :: (Arrow cat, Num c) => cat a c -> cat a c -> cat a c
addA'' f g = f &&& g >>> arr (uncurry (+))

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

-- (a -> (s, a))
runCircuit' :: Circuit a b -> [a] -> [b]
runCircuit' cir inputs =
  snd $ mapAccumL unCircuit cir inputs

-- accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
  let (output, acc') = f input acc
   in (accum acc' f, output)

accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = f a b in (b', b'))

total :: (Num a) => Circuit a a
total = accum' 0 (+)

-- [1, 1, 2, 2, 2, 4]
res1 :: [Integer]
res1 = runCircuit total [1, 0, 1, 0, 0, 2]

--  The purpose of the (.) function is to chain two arrows together from right to left. (>>>) and (<<<) are based on (.).
--  It needs to replace itself with the `dot` of the two replacements returned by the execution of the argument Circuits.

mean1 :: (Fractional a) => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

-- the compiler works out all the arr, (>>>), (&&&) stuff for you
mean2 :: (Fractional a) => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n

mean3 :: (Fractional a) => Circuit a a
mean3 = proc value -> do
  (t, n) <- (| (&&&) (total -< value) (total -< 1) |)
  returnA -< t / n

-- Hangman: Pick a word
generator :: (Random a) => (a, a) -> StdGen -> Circuit () a
generator range rng = accum rng $ \() rng' -> randomR range rng'

