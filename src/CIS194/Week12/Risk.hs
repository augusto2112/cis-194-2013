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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)


battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  let max_attackers = min 3 $ attackers bf - 1
  let max_defenders = min 2 $ defenders bf
  attacking <- liftM (sortBy (flip compare)) $ replicateM max_attackers die
  defending <- liftM (sortBy (flip compare)) $ replicateM max_defenders die
  let (defeats_attacking, defeats_defending) = calculateLosses $ zip attacking defending
  return $ Battlefield (attackers bf - defeats_attacking) (defenders bf - defeats_defending) 
    where 
      calculateLosses = foldl' (\(x, y) (x', y') -> if x' > y' then (x, y + 1) else (x + 1, y)) (0, 0)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do 
  updated@(Battlefield attackers defenders) <- battle bf
  if (attackers < 2) || (defenders <= 0) then return updated else invade updated

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  sims <- replicateM 1000 $ invade bf
  let victories = computeVictories sims
  return (victories / 1000)
  where
    computeVictories =  foldl' (\victories (Battlefield attackers _) -> 
      if attackers < 2 then victories else victories + 1) 0

test :: Show a => Rand StdGen a -> IO ()
test rand = evalRandIO rand >>= putStrLn . show