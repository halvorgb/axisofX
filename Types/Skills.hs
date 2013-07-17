module Types.Skills where

import Types.Items
import Types.Common

data WeaponConstraints = WeaponConstraints { wcWeight  :: [WeaponWeight],
                                             wcType    :: [WeaponType],
                                             wcGrip    :: [WeaponGrip]
                                           }
                         deriving (Eq)

data Skill = Active { sName :: String,
                      sDescription :: String,
                      sEffect :: [SkillEffect], -- allow multiple effects?
                      sTarget :: SkillTarget,
                      sPrequisites :: [Skill],
                      sSkillMask :: [SkillMask],
                      sWeaponConstraints :: WeaponConstraints,
                      
                      sEnergyCost :: Int,
                      sSpeedMultiplier :: Float,
                      
                      sCoolDown :: Int
                     -- MORE
                      
                     }
           | Sustained { sName :: String,
                         sDescription :: String,
                         sEffect :: [SkillEffect],
                         sTarget :: SkillTarget,
                         sPrequisites :: [Skill],
                         sSkillMask :: [SkillMask],
                         sWeaponConstraints :: WeaponConstraints,                         
                         sEnergyUpkeep :: Int
                         
                         -- MORE
                       }
             deriving(Eq)
             

data SkillEffect = Direct { seEffect :: Effect } -- heal/damage

                 | Delayed { seSkillEffect :: SkillEffect,
                             seDelay :: Int } -- skillEffect after a timer.
                   
                 | OverTime { seEffectPerTick :: Effect, -- also both heal and damage
                              seTimeBetweenTicks :: Int,
                              seDuration :: Int }
                   
--                 | Buff { seAttribute :: Attribute, -- buffs and debuffs.
--                          seValue :: Int,
--                          seDuration :: Int }
                   
                 deriving(Eq)        
                     
data SkillTarget = Self
                 | Other { stRange :: Int, 
                           stHitMask :: HitMask}
                 | Area  { stRange :: Int,
                           stRadius :: Int,
                           stHitMask :: HitMask
                         }
                 deriving(Eq)

                      
-- which skills an item or class allows.
-- By category?
data SkillMask = Brute | Finesse | Common | Clever | Shady 
               deriving(Show, Eq)