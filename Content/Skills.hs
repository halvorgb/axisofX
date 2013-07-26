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


-- For skills that do a constant amount of damage, regardless of weapon worn.
directHPLossFuncFactory :: Int -> SkillEffectFunction
directHPLossFuncFactory damage skill numberInQueue hitVal evdVal _ mitigation destEnt world =
  if energyCost > currentEnergy
  then
    world { wMessageBuffer = (skillMessage FAT skill hero destEnt):wMessageBuffer world}
  else
    let world' = world
    in world'
     
        
        --- PROBLEM: this will check for each and every entity if there is enough energy and potentially return a new world with updated energy/speed.
        -- doesn't work for AOE spells.
        -- SOLUTION: make these return a Maybe World, if said maybe is Nothing => return FAT message.
        -- NOPE! that doesn't work either, have a separate check after targets have been acquired.
        ---- If player has enough energy -> let world' with updated Energy/speed -> update said world' with this function (foldl')
    
    
  where
    hero = wHero world
    
    damageDealt = damage - mitigation

    

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
                    [ FinalConstant (directHPLossFuncFactory 1) 0 ],

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