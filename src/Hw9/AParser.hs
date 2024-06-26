{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Hw9.AParser () where

import Control.Applicative

import Control.Monad (void)
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
 where
  f [] = Nothing -- fail on the empty input
  f (x : xs) -- check if x satisfies the predicate
  -- if so, return x along with the remainder
  -- of the input (that is, xs)
    | p x = Just (x, xs)
    | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
 where
  f xs
    | null ns = Nothing
    | otherwise = Just (read ns, rest)
   where
    (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

-- here Parser a => a assume a is a function here
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (Parser p1) <*> (Parser p2) = Parser $ \s -> do
    (f, s') <- p1 s
    (g, s'') <- p2 s'
    return (f g, s'')

type Name = String
data Employee = Emp {name :: Name, phone :: String}

-- parseName :: Parser Name

-- parsePhone :: Parser String
-- parseName =

-- pattern: liftX :: Applicative f => (a -> b -> c -> d) -> f a -> b -> f c -> f d
--          liftX h fa b fc = h <$> fa <*> pure b <*> fc
-- _ = Emp <$> parseName <*> parsePhone

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

-- instance Applicative [] where
--   pure a = [a]
--   (f:fs) <*> as = (f <$> as) ++ (fs <*> as)
