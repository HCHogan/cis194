{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hw12.PlayGround (test) where

import Control.Monad.Random

threeInts :: Rand StdGen (Int, Int, Int)
threeInts = do
  a <- getRandom
  b <- getRandom
  c <- getRandom
  return (a, b, c)

threeInts' :: Rand StdGen (Int, Int, Int)
threeInts' =
  getRandom >>= \a ->
    getRandom >>= \b ->
      getRandom >>= \c ->
        return (a, b, c)

-- instance Monad ((->) e) where
--   return = const
--   m >>= k = \r -> k (m r) r

newtype MyState s a = MyState {runMyState :: s -> (a, s)}

myState :: (s -> (a, s)) -> MyState s a
myState = MyState

get :: MyState s s
get = MyState $ \s -> (s, s)

put :: s -> MyState s ()
put s = MyState $ const ((), s)

modify :: (s -> s) -> MyState s ()
modify f = MyState $ \s ->
  ((), f s)

instance Functor (MyState s) where
  fmap f (MyState g) = MyState $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (MyState s) where
  pure a = MyState (a,)
  (<*>) :: MyState s (a -> b) -> MyState s a -> MyState s b
  MyState f <*> MyState g = MyState $ \s ->
    let (h, s') = f s
        (a, s'') = g s'
     in (h a, s'')

instance Monad (MyState s) where
  return = pure
  (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
  MyState h >>= f = MyState $ \s ->
    let (a, newState) = h s
        (MyState g) = f a
     in g newState

type Counter = Int

increment :: MyState Counter ()
increment =
  get >>= \count ->
    put (count + 1)

example :: MyState Counter String
example = do
  _ <- increment
  _ <- increment
  _ <- increment
  count <- get
  return $ "Count is " ++ show count

test :: IO ()
test = do
  let initialState = 0
  let (result, _finalState) = runMyState example initialState
  putStrLn result
