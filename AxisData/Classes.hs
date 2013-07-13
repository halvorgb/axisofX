module AxisData.Classes where


import AxisData.Items.Weapons
import AxisData.Items.Inventory
import AxisData.Items.Armor


-- work on this, main idea: the player is a dick by default.
 -- Best reputation: Dick or asshole? Selfish?
data Reputation = Malevolent | Malicous | Hard 
                | Sociopath -- Neutral lol
                | Asshole
              deriving (Eq, Show)


data Class = 
  Class { cName :: String,
          cExpReqMultiplier :: Float, 
          cStartingHP :: Int,
          cHPGain :: Int,
          cStartingEnergy :: Int,
          cEnergyGain :: Int,
            
          cWield :: Weapon,
          cWear :: Armor,
          cInventory :: Inventory,
            
          cReputation :: Reputation
        }
  deriving (Eq)

instance Show Class where
  show c = show $ cName c
           
           
            