module Content.StartingGear where

import Types.Common
import Types.Items

-- Starting weapons: Not randomly generated.
shortSword = 
  Weapon {
    wepQuality = JourneyMan,
    wepWeight = Balanced,
    wepType = Edged,
    wepGrip = OneHanded,
    wepLevel = 1,
    
    wepDamageDie = Dice { dDie = (1, 6), dMod = 0},
    wepHitBonus = 0,
    wepSpeedMultiplier = 1.0,
    wepRange = 1
    }

rags = Armor { aEvasion = 0,
               aMitigation = 0,
               aDescription = "RAGS!"
             }
       
