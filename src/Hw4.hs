module Hw4
  ( fun1,
    fun2,
    foldTree,
    xor,
    map',
    myFoldl,
  )
where

-- 1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- 2
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf
  where
    insertNode x Leaf = Node 0 Leaf x Leaf
    insertNode x (Node _ left val right)
      | height left <= height right = let newLeft = insertNode x left in Node (height newLeft + 1) newLeft val right
      | otherwise = let newRight = insertNode x right in Node (height newRight + 1) left val newRight
      where
        height Leaf = 0
        height (Node h _ _ _) = h

-- 3
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr (\b g a -> g (f a b)) id xs base
myFoldl f x = foldr (flip f) x . reverse

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

class Eq' a where
  eq :: a -> a -> Bool
  neq :: a -> a -> Bool
  neq x y = not $ eq x y

instance Eq' Foo' where
  eq (F' x) (F' y) = x == y
  eq (G' x) (G' y) = x == y
  eq _ _ = False
