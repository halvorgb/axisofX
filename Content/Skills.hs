module Content.Skills where

import Types.World
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
onlyBurdensome = 
  WeaponConstraints {wcWeight = [Burdensome],
                     wcType = anyType,
                     wcGrip = anyGrip
                    }



directFunctionCreator :: Int -> (Entity -> Entity)
directFunctionCreator n = (\e -> e {eCurrHP = eCurrHP e + n})




-- Actual Skills:
sweep = Active  { sName = "Sweep",
                  sShortName = "Sweep",
                  sDescription = "Attacks every enemy in weapon range of the player.",
                  sEffect = 
                    [Final { seEffect = directFunctionCreator (-5)
                           , seDelay = 0 }],
                  sTarget =
                   Area { stRange = SRConst 0, 
                          stRadius = SRWeaponRange,
                          stHitMask = Enemies 
                        },
                            
                  sPrequisites = [],
                  sSkillMask = [Brute],
                  
                  sWeaponConstraints = anyWeapon,                  
                  
                  sEnergyCost = 8,
                  sSpeedMultiplier = 1.5,
                  sCoolDown = 15
                } 