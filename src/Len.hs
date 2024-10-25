{-# LANGUAGE RankNTypes #-}

module Len () where

data Foo = Foo {bar :: (Int, Int), baz :: Char}
  deriving (Show)

getBar :: Foo -> (Int, Int)
getBar = bar

setBar :: Foo -> (Int, Int) -> Foo
setBar m x = m{bar = x}

get_2 :: (a, b) -> b
get_2 = snd

set_2 :: (a, b) -> b -> (a, b)
set_2 (a, _) b = (a, b)

myTuple :: (Int, Int)
myTuple = (1, 1)

myFoo :: Foo
myFoo = Foo myTuple 'a'

type Lens s a = forall f . Functor f => (a -> f a) -> s -> f s -- promote record update function to structure update function

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens sa sas aa s = sas s $ aa (sa s)

view :: Lens s a -> s -> a
view l s = undefined $ l id s

set :: Lens s a -> a -> s -> s
set l a = l (const a)

barL :: Lens Foo (Int, Int)
barL = lens bar $ \m x -> m{bar = x}

_2 :: Lens (a, b) b
_2 = lens snd $ \(a, _) b -> (a, b)

main :: IO ()
main = do
  print $ view _2 myTuple
  print $ set (barL . _2) 10 myFoo
