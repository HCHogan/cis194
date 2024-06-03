{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Hw6 (fib, fibs1, fibs2, streamToList, streamRepeat, streamMap, streamFromSeed, test1, test2) where

import Prelude hiding ((/))

fib :: Integer -> Integer
fib x
  | x == 0 = 0
  | x == 1 = 0
  | otherwise = fib (x - 1) + fib (x - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, b + a)) (0, 1)

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

instance (Show a) => Show (Stream a)
show :: (Show a) => Stream a -> [Char]
show s = show' 20 s
 where
  show' n (Stream a s)
    | n == 0 = ""
    | otherwise = Prelude.show a ++ [' '] ++ show' (n - 1) s

streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = Stream (f a) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f i = let j = f i in Stream j $ streamFromSeed f j

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a s) (Stream b t) = Stream a $ Stream b $ interleaveStreams s t

nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) nats

-- data Stream a = Stream a (Stream a)
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Stream n $ streamRepeat 0
  negate (Stream a s) = Stream (-a) $ negate s
  (+) (Stream a0 s) (Stream b0 t) = Stream (a0 + b0) $ s + t
  (*) (Stream a0 s) b@(Stream b0 t) = Stream (a0 * b0) $ streamMap (* a0) t + s * b

(/) :: Stream Integer -> Stream Integer -> Stream Integer
(/) (Stream a0 s) (Stream b0 t) = q
 where
  q = Stream (a0 `div` b0) $ streamMap (`div` b0) (s - q * t)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

test1 :: IO ()
test1 = do
  putStrLn $ Hw6.show $ x ^ 4
  putStrLn $ Hw6.show $ (1 + x) ^ 5
  putStrLn $ Hw6.show $ (x ^ 2 + x + 3) * (x - 5)

test2 :: IO ()
test2 = do
  putStrLn $ Hw6.show fibs3
