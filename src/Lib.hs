module Lib (
    someFunc,
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Thing
    = Shoe
    | Ship
    | SealingWax
    | Cabbage
    | King
    deriving (Show)

