module Types.Classes where

import Types.Common
import Types.Items

-- work on this, main idea: the player is a dick by default.
 -- Best reputation: Dick or asshole? Selfish?
data Reputation = Malevolent | Malicous | Hard 
                | Sociopath -- Neutral lol
                | Asshole
              deriving (Eq, Show)


data Class = 
  Class { cName :: String,
          cExpReqMultiplier :: Float, 
          cStartingHPMultiplier :: Float,
          cHPPerLevelMultiplier :: Float,
          cStartingEnergyMultiplier :: Float,
          cEnergyPerLevelMultiplier :: Float,
            
          cStartingWeapon :: Weapon,
          cStartingArmor :: Armor,
          cStartingInventory :: Inventory,
            
          cStartingReputation :: Reputation,
          
          cHitDie :: Dice,
          cEvadeDie :: Dice,
          cDamageBonus :: Int,
          cMitigationBonus :: Int
          
          -- more attributes such as allowed weapons and skills.
        }
  deriving (Eq)

instance Show Class where
  show c = filter (/= '\"') $ show $ cName c