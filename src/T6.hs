module T6 (fuck) where

import Data.Maybe (fromMaybe)

_ = take 3 $ repeat 7

_ = foldl (+) 0 [1, 2, 3]

findRange :: (Ord a) => (a, a) -> [a] -> Maybe (Int, Int)
findRange (a, b) xs = toTuple <$> go xs 0 (length xs - 1) Nothing Nothing
 where
  toTuple (low, high) = (low, high)

  go _ l r low high
    | l > r = case (low, high) of
        (Just lo, Just hi) -> Just (lo, hi)
        _ -> Nothing
    | otherwise =
        let m = (l + r) `div` 2
            current = xs !! m
         in if current >= a && current < b
              then go xs (m + 1) (m - 1) (Just $ fromMaybe m low) (Just $ fromMaybe m high)
              else
                if current >= b
                  then go xs l (m - 1) low high
                  else go xs (m + 1) r low high

-- example to test the function
fuck :: IO ()
fuck = print $ findRange (48, 51) [21, 31, 34, 50, 62, 70]

-- knapsack01 ::
--   [Double] -> -- values
--   [Integer] -> -- nonnegative weights
--   Integer -> -- knapsack size
--   Double -- max possible value
-- knapsack01 vs ws maxW = m ! (numItems - 1, maxW)
--  where
--   numItems = length vs
--   m =
--     array ((-1, 0), (numItems - 1, maxW)) $
--       [((-1, w), 0) | w <- [0 .. maxW]]
--         ++ [((i, 0), 0) | i <- [0 .. numItems - 1]]
--         ++ [ ((i, w), best)
--            | i <- [0 .. numItems - 1]
--            , w <- [1 .. maxW]
--            , let best
--                   | ws !! i > w = m ! (i - 1, w)
--                   | otherwise =
--                       max
--                         (m ! (i - 1, w))
--                         (m ! (i - 1, w - ws !! i) + vs !! i)
--            ]
--
-- example = knapsack01 [3, 4, 5, 8, 10] [2, 3, 4, 5, 9] 20

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = smaller ++ [x] ++ bigger
 where
  smaller = quicksort $ filter (<= x) xs
  bigger = quicksort $ filter (> x) xs
