{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

instance Monoid Battlefield where
  mempty = Battlefield 0 0
  mappend b1 b2 = Battlefield (attackers b1 + attackers b2) (defenders b1 + defenders b2)

maxAtk :: Battlefield -> Int
maxAtk bf = min 3 (attackers bf - 1)

maxDef :: Battlefield -> Int
maxDef bf = min 2 (defenders bf)

makeRolls :: Int -> Rand StdGen [Int]
makeRolls 0 = return []
makeRolls n = do
                d <- die
                r <- makeRolls (n - 1)
                return $ (unDV d) : r

calcDiff :: [Int] -> [Int] -> Battlefield
calcDiff a d = let  sa = reverse (sort a)
                    sd = reverse (sort d)
                    res = zipWith (\a d -> if a > d then Battlefield 0 (-1) else Battlefield (-1) 0) sa sd
                in foldl (\a b -> a `mappend` b) mempty res

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
              let atk = maxAtk bf
                  def = maxDef bf
              rollsAtk <- makeRolls atk
              rollsDef <- makeRolls def
              let res = calcDiff rollsAtk rollsDef
              return $ bf `mappend` res