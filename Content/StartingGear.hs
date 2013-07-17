module Content.StartingGear where

import  Types.Items

-- Starting weapons: Not randomly generated.
shortSword = 
  Weapon {
    wepQuality = JourneyMan,
    wepWeight = Balanced,
    wepType = Edged,
    wepGrip = OneHanded,
    wepLevel = 1,
    
    wepDescription = "Dull Short Sword (dmg: 1d6, hitPenalty: (-0), spdMultiplier: 1.0), level 1 crafted by a Journeyman.",
    wepDamageDie = (1, 6),
    wepHitPenalty = 0,
    wepSpeedMultiplier = 1.0
    }

rags = Armor { aAvoidance = 0,
               aMitigation = 0,
               aDescription = "RAGS!"
             }