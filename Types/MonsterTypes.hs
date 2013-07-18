module Types.MonsterTypes where

import System.Random

import Types.Common

data MonsterType =
  MonsterType { mtName :: String,
                mtExpRewardMultiplier :: Float,
                mtHPMultiplier :: Float,
                
                mtSpeedMultiplier :: Float,
--                mtSkills = [Skills],
                -- levelConstraints :: (Int, Int),                
                -- RaceConstraints?
                
                mtHitDie :: Dice,
                mtEvadeDie :: Dice,
                mtDamageDie :: Dice,
                mtMitigation :: Int
              }
  deriving(Eq)
instance Show MonsterType where
  show mt = show $ mtName mt
