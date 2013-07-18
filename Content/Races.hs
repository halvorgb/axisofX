module Content.Races where

import Types.World

vanilla :: World -> World
vanilla w = w

-- add new races here
races = [human, ogre]

-- race definitions.
human = Race { rName = "Human",
               rHitModifier = 0,
               rEvasionModifier = 0,
               rDamageModifier = 0,
               rMitigationModifier = 0,
               
               rBaseSpeed = 10,
               
               rBaseHP = 10,
               rBaseHPPerLevel = 2,
               
               rBaseEnergy = 50,
               rBaseEnergyPerLevel = 2,
               rBaseEnergyCost = 1,
               
               rExperiencePenalty = 1.0,
               
               rContextFunc = vanilla
             }
        
ogre = Race { rName = "Ogre",
              rHitModifier = (-1),
              rEvasionModifier = (-5),
              rDamageModifier = 3,
              rMitigationModifier = 4,
              
              rBaseSpeed = 20,
              
              rBaseHP = 20,
              rBaseHPPerLevel = 4,
              
              rBaseEnergy = 40,
              rBaseEnergyPerLevel = 2,
              rBaseEnergyCost = 1,
              
              rExperiencePenalty = 1.0,
              
              rContextFunc = vanilla
            }