module Content.StartingGear where

import Types.Common
import Types.Items

import ItemGeneration

-- Starting weapons: Not randomly generated.
shortSword =
  generateWeaponStats Balanced Edged OneHanded 1 JourneyMan


rags = Armor { aEvasion = 0,
               aMitigation = 0,
               aDescription = "RAGS!"
             }
       
