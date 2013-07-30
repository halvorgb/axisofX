module Content.Classes where

import Content.StartingGear
import Content.Skills

import Types.Common
import Types.World
import Types.Items


classes = [berserker]

peasant = Class { cName = "Peasant",
                  
                  cExpReq = 100,
                  cBaseHP = 10,
                  cBaseSpeed = 5,
                  cHPPerLevel = 2,
                  cBaseEnergy = 100,
                  cEnergyPerLevel = 5,
                  
                  cStartingWeapon = shortSword,
                  cStartingArmor = rags,
                  cStartingInventory = Inventory [] 0,
                  cStartingSkills = [],
                  
                  cStartingReputation = Asshole,
                  
                  cHitDie = Dice {dDie = (1,20), dMod = 0},
                  cEvadeDie = Dice {dDie = (1,20), dMod = 0},
                  cDamageBonus = 0,
                  cMitigationBonus = 0,
                  
                  cSkillMask = anySkillMask,
                  cWeaponConstraints = anyWeapon
                                        
                                        
                }

berserker = Class { cName = "Berserker",
                    
                    cExpReq = 120,
                    cBaseHP = 100,
                    cBaseSpeed = 2,
                    cHPPerLevel = 10,
                    cBaseEnergy = 100,
                    cEnergyPerLevel = 10,
                    
                  
                    cStartingWeapon = shortSword,
                    cStartingArmor = rags,
                    cStartingInventory = Inventory [] 0,
                    cStartingSkills = [sweep, leap],
                    
                    cStartingReputation = Asshole,
                    
                    cHitDie = Dice {dDie = (1,20), dMod = 0},
                    cEvadeDie = Dice {dDie = (1,20), dMod = 0},
                    cDamageBonus = 2,
                    cMitigationBonus = 0,
                    
                    cSkillMask = anySkillMask,
                    cWeaponConstraints = anyWeapon
                                        
                                        
                }
                    
                    