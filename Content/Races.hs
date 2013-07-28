module Content.Races where

import Types.World
import Types.Common

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
               
               rSpeedMultiplier = 1.0,
               rHPMultiplier = 1.0,
               rEnergyMultiplier = 1.0,
               rExperienceMultiplier = 1.0,               
               
               rContextFunc = vanilla,
               rSkillMask = []
               
             }
        
ogre = Race { rName = "Ogre",
              rHitModifier = -1,
              rEvasionModifier = -5,
              rDamageModifier = 3,
              rMitigationModifier = 4,
              
              rSpeedMultiplier = 1.0,
              rHPMultiplier = 2.0,
              rEnergyMultiplier = 2.0,
              rExperienceMultiplier = 2.0,               
              
              
              rContextFunc = vanilla,
              rSkillMask = []
            }