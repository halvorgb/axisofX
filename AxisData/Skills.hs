module AxisData.Skills where


import AxisData.Attributes
import AxisData.Common
import AxisData.Items.Weapons


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
                   
                 | Buff { seAttribute :: Attribute, -- buffs and debuffs.
                          seValue :: Int,
                          seDuration :: Int }
                   
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
                      

-- skills this model should allow:
{-

Skills that affect multiple enemies across multiple tiles,
Skills that affect multiple enemies on the same tile,
^... both active and passively


-}

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