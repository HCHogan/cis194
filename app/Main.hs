module Main (main) where

import Data.List (foldl')
import Hw4 (foldTree)
import T6 (fuck)

sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

hailstone :: (Integral a) => a -> a
hailstone n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

ff = fst ("haskell", 0)

add a b = a + b

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

myMap func [] = []
myMap func (x : xs) = func x : myMap func xs

_ = [1 .. 5] |> myMap (\x -> x * 2)

divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just (x `div` y)

safeDivide :: Int -> Int -> Int -> Maybe Int
safeDivide x y z = do
  a <- divide x y
  b <- divide a z
  return b

main :: IO ()
-- main = print (safeDivide 10 2 0)
main = do
  fuck
  putStrLn $ show $ foldTree "ABCDEFGHIJ"
  putStrLn "Starting..."
  putStr "Enter a number: "
  numStr <- getLine
  let num = read numStr :: Int
  putStrLn ("You entered: " ++ show num)
  putStrLn "Done."

_ = getLine >>= \name -> putStrLn ("Hello, " ++ name ++ "!")

qsort [] = []
qsort (x : xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (> x) xs)

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x : xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 1
product' (x : xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

_ = foldl' (const (1 +)) 0 [1, 2]


