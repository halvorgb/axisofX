module Content.Skills where

import Types.Skills
import Types.Items
import Types.Common



-- Some lists to make skill making easier.
anyWeight :: [WeaponWeight]
anyWeight = [Balanced, Heavy, Burdensome]

anyType :: [WeaponType]
anyType = [Edged, Pointy, Blunt]

anyGrip :: [WeaponGrip]
anyGrip = [OneHanded, TwoHanded]

anyWeapon = 
  WeaponConstraints {wcWeight = anyWeight,
                     wcType = anyType,
                     wcGrip = anyGrip
                    }
  


-- Actual Skills:
sweep = Active  { sName = "Sweep",
                  sDescription = "Sweeps through every target on an adjecent tile",
                  sEffect = 
                    [Direct { seEffect = (Harm 5) }], -- allow multiple effects?
                  sTarget =
                    Other { stRange = 1, stHitMask = Enemies },
                            
                  sPrequisites = [],
                  sSkillMask = [Brute],
                  
                  sWeaponConstraints = anyWeapon,

                  
                  
                  
                  sEnergyCost = 8,
                  sSpeedMultiplier = 1.5,
                  sCoolDown = 15
                } 