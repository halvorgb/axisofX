module AxisData.Monsters where

import AxisData.Skills


-- This is not yet in use. for the future.


data MonsterType = 
  MonsterType { mtName :: String,
                mtHPMultiplier :: Int,
                mtBaseHP :: Int,
                mtSkills :: [Skill]
              }
  deriving(Eq)
          
instance Show MonsterType where
  show mt = show $ mtName mt