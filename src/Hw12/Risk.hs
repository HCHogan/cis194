{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hw12.Risk (die, dice, battle, invade, successProb) where

import Control.Monad
import Control.Monad.Random
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  let att = min 3 (attackers bf - 1)
      def = min 2 (defenders bf)
   in do
        attRolls <- dice att
        defRolls <- dice def
        let attRolls' = sortBy (comparing Down) attRolls
            defRolls' = sortBy (comparing Down) defRolls
            pairs = zip attRolls' defRolls'
            results = map (\(a, d) -> if a > d then 0 else 1) pairs
            attLosses = sum results
            defLosses = length results - attLosses
        return $ Battlefield (attackers bf - attLosses) (defenders bf - defLosses)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  bf' <- battle bf
  if attackers bf' < 2 || defenders bf' == 0
    then return bf'
    else invade bf'

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  let wins = length $ filter (\b -> defenders b == 0) results
  return $ fromIntegral wins / 1000
