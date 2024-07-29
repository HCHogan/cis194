{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Linear where

import System.Exit (exitFailure)
import Text.Read (readMaybe)

data Ur a where
  Ur :: a -> Ur a

(&) :: a %1 -> (a %1 -> b) %1 -> b
x & f = f x

-- data X = A | B {name :: String} | C {x::Int, y::Int, name::String}
--
-- myfn :: X %1 -> Int
-- myfn A = 1
-- myfn B{name} = length name
-- myfn (C {x=x, y=y, name=name}) = x + y

data MyRecord where
  MyRecord :: {field1 :: Int, field2 :: String, field3 :: Bool} -> MyRecord

accessFields :: MyRecord %1 -> (Int, String)
accessFields (MyRecord{field1, field2}) = (field1, field2)

-- accessTwoFields :: MyRecord %1 -> (Int, String, MyRecord)
-- accessTwoFields r = (field1 r, field2 r, r)
