module Content.StartingGear where

import Types.Common
import Types.Items

import ItemGeneration

-- Starting weapons: Not randomly generated.
shortSword =
  generateWeaponStats Balanced Edged OneHanded 1 JourneyMan
  
bestMaul =
  generateWeaponStats Balanced Blunt TwoHanded 1 $ GrandMaster { wTitle = "Face Surgeon", wLegacy = "Savior of Widows" }


rags = Armor { aEvasion = 0,
               aMitigation = 0,
               aDescription = "RAGS!"
             }
       
