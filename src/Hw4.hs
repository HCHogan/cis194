module Hw4 (

) where

import Prelude

-- 1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

-- 2
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)
