{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module T11 (employees1, employees2, example) where

type Name = String

data Employee = Employee
  { name :: Name
  , phone :: String
  }
  deriving (Show)

names :: [String]
names = ["Joe", "Sara", "Mae"]

phones :: [String]
phones = ["555-5555", "123-456-7890", "555-4321"]

employees1 :: [Employee]
employees1 = Employee <$> names <*> phones

newtype FuckList a = FuckList {getFuckList :: [a]}
  deriving (Eq, Show, Functor)

instance Applicative FuckList where
  pure = FuckList . repeat
  FuckList fs <*> FuckList xs = FuckList (zipWith ($) fs xs)

employees2 :: [Employee]
employees2 = getFuckList $ Employee <$> FuckList names <*> FuckList phones

addOne :: Int -> Int -> String
addOne x y = show (x + y + 1)

mulByTwo :: Int -> Int
mulByTwo x = x * 2

example :: String
example = (addOne <*> mulByTwo) 4

data BigRecord = BR
  { getName :: Name
  , getSSN :: String
  , getSalary :: Integer
  , getPhone :: String
  , getLicensePlate :: String
  , getNumSickDays :: Int
  }

r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2

getEmp :: BigRecord -> Employee
getEmp = Employee <$> getName <*> getPhone

ex01 = getEmp r

pair :: (Applicative f) => f a -> f b -> f (a, b)
pair fa fb = (,) <$> fa <*> fb

{-
(*>) :: Applicative f => f a -> f b -> f b
这个操作符的作用是运行第一个 f a，然后运行第二个 f b，最后返回第二个 f b 的结果。

如果 f 是 Maybe，那么 Nothing *> Just x = Nothing， Just _ *> Nothing = Nothing， Just _ *> Just x = Just x。
如果 f 是 []，那么 lst1 *> lst2 会计算两个列表的笛卡尔积，并返回第二个列表的值作为结果。例如 [1, 2] *> [3, 4] 将计算为 [3, 4, 3, 4]。
如果 f 是 ZipList，这个操作与 zip 函数相同，但会丢弃第一个列表的值。例如 ZipList [1, 2] *> ZipList [3, 4] 结果是 ZipList [3, 4]。
如果 f 是 IO，这个操作符会按顺序执行两个 IO 动作，但丢弃第一个动作的结果。例如 putStrLn "Hello" *> putStrLn "World" 将输出 Hello World。
如果 f 是 Parser，这个操作符会顺序运行两个解析器，但丢弃第一个解析器的结果，而只返回第二个解析器的结果。如果任何一个解析器失败，整个操作也会失败。

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA 是将输入列表中的每个元素应用一个产生 f b 的函数，然后返回一个包含这些 f b 的 f [b]。

如果 f 是 Maybe，那么 mapA 对应于所有输入都是 Just 的情况下返回 Just 包装的输出列表，否则返回 Nothing。
如果 f 是 []，类似于 classic 的 mapM，会生成所有可能的组合（即生成包含所有可能的 [b] 的列表）。
如果 f 是 ZipList，它会作用于两个 ZipList，产生一对一映射。
如果 f 是 IO，则会对列表中的每个元素执行某个 IO 操作，并将结果收集到一个列表中。
如果 f 是 Parser，会对输入应用解析器， 如果任何一个解析失败，整个操作也会失败。

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA 是将一个装有 f a 元素的列表转换成一个包含列表的 f。

如果 f 是 Maybe，当任何一个元素为 Nothing 时，返回 Nothing，否则返回 Just 包装的输出列表。
如果 f 是 []，会生成所有可能的组合（即生成包含所有可能的 [a] 的列表）。
如果 f 是 ZipList，它会组合相应位置的元素。
如果 f 是 IO，将会按顺序执行每个 IO 动作并收集结果。
如果 f 是 Parser，对每个解析器按顺序执行，返回所有解析结果组成的列表。如果任何一个解析器失败，整个操作也会失败。

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 会运行一个 f a 多次，并返回一个包含 a 结果的 f [a]。

如果 f 是 Maybe，当第一次就失败时，之后的操作都不会进行。
如果 f 是 []，类似于像笛卡尔积一样生成所有可能的组合。
如果 f 是 ZipList，它会产生一个包含重复元素构成的 ZipList。
如果 f 是 IO，会多次运行一个 IO 操作，并收集所有运行结果。
如果 f 是 Parser，会重复执行某个解析器，如果任何一次失败，整个操作会失败。
这些函数都使用了 Applicative 的概念，通过排列组合方式来解释不同 Applicative 实例的行为。
 - -}
