module Content.Classes where

import Content.StartingGear

import Types.Common
import Types.Classes
import Types.Items

classes = [peasant]

peasant = Class { cName = "Peasant",
                  
                  cExpReqMultiplier = 1.0,
                  cStartingHPMultiplier = 1.0,
                  cHPPerLevelMultiplier = 1.0,
                  cStartingEnergyMultiplier = 1.0,
                  cEnergyPerLevelMultiplier = 1.0,

                  
                  cStartingWeapon = shortSword,
                  cStartingArmor = rags,
                  cStartingInventory = Inventory [] 0,
                  
                  cStartingReputation = Asshole,
                  
                  cHitDie = Dice {dDie = (1,20), dMod = 0},
                  cEvadeDie = Dice {dDie = (1,20), dMod = 0},
                  cDamageBonus = 0,
                  cMitigationBonus = 0
                                        
                                        
                }