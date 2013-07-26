module Content.Classes where

import Content.StartingGear
import Content.Skills

import Types.Common
import Types.World
import Types.Items


classes = [peasant, berserker]

peasant = Class { cName = "Peasant",
                  
                  cExpReqMultiplier = 1.0,
                  cStartingHPMultiplier = 1.0,
                  cHPPerLevelMultiplier = 1.0,
                  cStartingEnergyMultiplier = 1.0,
                  cEnergyPerLevelMultiplier = 1.0,

                  
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
                    
                    cExpReqMultiplier = 1.2,
                    cStartingHPMultiplier = 10,
                    cHPPerLevelMultiplier = 1.5,
                    cStartingEnergyMultiplier = 1.0,
                    cEnergyPerLevelMultiplier = 1.0,
                    
                  
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
                    
                    