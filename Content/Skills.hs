module Content.Skills where

import Data.Maybe

import Types.World
import Types.Items
import Types.Common

import Helpers

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



-- SkillEffects
directHPLossFuncFactory :: Int -> (Entity -> Entity)
directHPLossFuncFactory n = (\e -> e {eCurrHP = eCurrHP e + n})

movementFuncFactory :: Int -> (Entity -> Entity)
movementFuncFactory n = (\e -> e {eCurrPos = eOldPos e |+| (n,0),  eOldPos = eCurrPos e})

-- TODO: Scaling functions (with damage die etc.)
-- TODO: Functions that do an effect with modified dies.


-- Target Functions
selectHero :: World -> [Entity]
selectHero w = [wHero w]

-- not used yet.
selectEntityFuncFactory :: Int -> World -> [Entity]
selectEntityFuncFactory dir w = [h]
  where
    h = wHero w

selectEntitiesFromRadius :: Position -> Int -> World -> [Entity]
selectEntitiesFromRadius pos rad w =
  concat $ map (\p -> case  (getEntityFromPos p w) of
                   Nothing -> []
                   Just e -> [e]
               ) positions
  where   
--    positions = pos - rad, pos - (rad-1), ..., pos, ..., pos + (rad-1), pos + rad
    positions = 
      map (\l -> (fst pos + l, 0)) [(-rad)..rad]
      
      


-- AOE Weapon radius selector
selectAOEWeaponRadius :: World -> [Entity]
selectAOEWeaponRadius w =
  selectEntitiesFromRadius (eCurrPos h) wepR w
  where
    h = wHero w
    wepR  = wepRange $  hWield h





-- Actual Skills:
sweep = Active  { sName = "Sweep",
                  sShortName = "Sweep",
                  sDescription = "Attacks every enemy in weapon range of the player.",
                  sEffect =
                    FinalScaling { seAffectedStat = Stat_HP,
                                   seScale = 0.75,
                                   seDelay = 0
                                 }
                  sTarget = selectAOEWeaponRadius,

                            
                  sPrequisites = [],
                  sSkillMask = [Brute],
                  
                  sWeaponConstraints = anyWeapon,                  
                  sHitMask = Enemies,
                  
                  sEnergyCost = 8,
                  sSpeedMultiplier = 1.5,
                  sCoolDown = 15
                }
{-        
leap = Active { sName = "Leap",
                sShortName = "Leap",
                sDescription = "Leaps forward, skipping one tile. Must land on free tile.",
                
                sEffect = 
                  [Final {seEffect = 
-}