module AxisData.Items.Inventory where


import AxisData.Items.Weapons
import AxisData.Items.Armor

type Gold = Int

data Potion = Potion { pAmount :: Int,
                       pDesc :: String,
                       pEffect :: PotionEffect } 
            deriving (Show, Eq)
                     
data PotionEffect = Heal | Harm
                  deriving (Show, Eq)
                           
data Item = Arm Armor | Pot Potion | Weap Weapon
          deriving(Show, Eq)
                     
data Inventory = Inventory [Item] Gold
               deriving(Show, Eq)