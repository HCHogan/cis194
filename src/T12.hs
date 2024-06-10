module T12 () where

check :: Int -> Maybe Int
check n
  | n < 10 = Just n
  | otherwise = Nothing

halve :: Int -> Maybe Int
halve n
  | even n = Just $ n `div` 2
  | otherwise = Nothing

ex01 :: Maybe Int
ex01 = check 7 >>= halve
ex02 :: Maybe Int
ex02 = check 12 >>= halve
ex03 :: Maybe Int
ex03 = halve 12 >>= check

mySequence2 :: (Monad m) => [m a] -> m [a]
mySequence2 [] = return []
mySequence2 (ma : mas) =
  ma >>= \a ->
    mySequence2 mas >>= \as ->
      return (a : as)

mySequence :: (Monad m) => [m a] -> m [a]
mySequence [] = return []
mySequence (ma : mas) = do
  a <- ma
  as <- mySequence mas
  return (a : as)

