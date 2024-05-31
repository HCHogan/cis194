module Hw5 () where

import Parser (parseExp)

data ExprT
  = Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr input = case parseExp Lit Add Mul input of
  Just expr -> Just (eval expr)
  Nothing -> Nothing
