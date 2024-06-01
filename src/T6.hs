module T6 () where

_ = take 3 $ repeat 7

_ = foldl (+) 0 [1, 2, 3]

findRange :: Ord a => [a] -> (a, a) -> (Int, Int)
findRange xs (a, b) = 
