module Content.Classes where

import Content.StartingGear

import Types.Player
import Types.Items

peasant = Class { cName = "Peasant",
                  
                  cExpReqMultiplier = 1.0,
                  cStartingHPMultiplier = 1.0,
                  cHPPerLevelMultiplier = 1.0,
                  cStartingEnergyMultiplier = 1.0,
                  cEnergyPerLevelMultiplier = 1.0,

                  
                  cStartingWeapon = shortSword,
                  cStartingArmor = rags,
                  cStartingInventory = Inventory [] 0,
                  
                  cStartingReputation = Asshole
                }