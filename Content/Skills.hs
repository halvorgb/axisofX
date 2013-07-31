module Content.Skills where

import Data.Maybe

import Types.World
import Types.Items
import Types.Common

import Helpers
import Messages
import Combat

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
directHPLossFuncFactory damage skill hitVal evdVal _ mitigation destEnt world =
  if hitVal > evdVal -- hit!
  then
    let damageAfterMit = damage - mitigation
    in if damageAfterMit > 0
       then
         damageEntity hero destEnt damageAfterMit $ world {wMessageBuffer = skillMessage SUCC skill hero destEnt:wMessageBuffer world }
       else
         world {wMessageBuffer = skillMessage MIT skill hero destEnt:wMessageBuffer world}
  else
    world { wMessageBuffer = skillMessage MISS skill hero destEnt:wMessageBuffer world }    
  where
    hero = wHero world
    
    damageDealt = damage - mitigation

    

leapFuncFactory :: Int -> SkillEffectFunction
leapFuncFactory distance skill _ _ _ _ destEnt world =
  case moveHero desiredPosition world of
    Just world' -> world' { wMessageBuffer = skillMessage SUCC skill hero destEnt:wMessageBuffer world }
    Nothing     -> world  { wMessageBuffer = skillMessage (FAIL CantReach) skill hero destEnt:wMessageBuffer world }
  where
    hero = wHero world
    desiredPosition = eCurrPos hero |+| (distance, 0)
    

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
  concatMap (\p -> case getEntityFromPos p w of
                Nothing -> []
                Just e -> [e]
            ) positions
  where   
--    positions = pos - rad, pos - (rad-1), ..., pos, ..., pos + (rad-1), pos + rad
    (vFmin, vFmax) = getViewFrame w
    positions = 
      map (\l -> (fst pos + l, 0)) [(negate rad)..(min vFmax rad)]
      
      


-- AOE Weapon radius selector
selectAOEWeaponRadius :: World -> [Entity]
selectAOEWeaponRadius w =
  selectEntitiesFromRadius (eCurrPos h) wepR w
  where
    h = wHero w
    wepR = wepRange $  hWield h



-- Actual Skills:
sweep = Active  { sName = "Sweep",
                  sShortName = "Sweep",
                  sDescription = "Attacks every enemy in weapon range of the player.",
                  sEffect = 
                    [ FinalConstant $ directHPLossFuncFactory 100 ],

                  sTarget = selectAOEWeaponRadius,

                  sPrequisites = [],
                  sSkillMask = [Brute],
                  
                  sWeaponConstraints = anyWeapon,                  
                  sHitMask = Enemies,
                  
                  sEnergyCost = 2,
                  sSpeedMultiplier = 1.5,
                  sCoolDown = 15
                }


leap = Active { sName = "Leap",
                sShortName = "Leap",
                sDescription = "Leaps forward, skipping one tile. Must land on free tile.",
                
                sEffect = 
                  [ FinalConstant $ leapFuncFactory 2 ],
                sTarget = selectHero,
                
                sPrequisites = [],
                sSkillMask = [Brute],
                sWeaponConstraints = anyWeapon,
                sHitMask = All,
                
                sEnergyCost = 5,
                sSpeedMultiplier = 2,
                sCoolDown = 1000000
              }
