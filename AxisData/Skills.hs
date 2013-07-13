module AxisData.Skills where


import AxisData.Attributes
import AxisData.Common

data Skill = Active { sName :: String,
                      sDescription :: String,
                      sEffect :: [SkillEffect], -- allow multiple effects?
                      sTarget :: SkillTarget,
                      sPrequisites :: [Skill],
                      sSkillMask :: [SkillMask],
                      
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
                         
                         sEnergyUpkeep :: Int
                         
                         -- MORE
                       }
             deriving(Eq)
             

data SkillEffect = Direct { seValue :: Int } -- heal/damage
                 | OverTime { seValuePerTick :: Int, -- also both heal and damage
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