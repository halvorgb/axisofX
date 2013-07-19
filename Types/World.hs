module Types.World where

import qualified Data.Map as M
import System.Random

import Types.Common
import Types.MonsterTypes
import Types.Classes
import Types.Items
import Types.Tiles


-- Level&World
---------------------------------------
data Level = Level { lDepth :: Int,
                     lGold :: M.Map Position Int,
                     lItems :: M.Map Position [Item],
                     lSize :: Int,
                     lFloorTiles :: M.Map Position FloorTile,
                     lWallTiles :: M.Map Position WallTile,                     
                     lEntities :: M.Map Position Entity }
           deriving (Show)

data World = World { wDepth :: Int,
                     wHero :: Entity,
                     wLevel :: Level,
                     wLevels :: [Level], 
                     wPrevInput :: Input, 
                     wMessageBuffer :: [String],
                     wStdGen :: StdGen,
                     wBoss :: Entity,
                     wScreenShown :: Screen
                   }
             
           deriving (Show)
-------------------------------------


-- Entities:           
-------------------------------------
data Entity = Monster { mType :: MonsterType,
                        mRace :: Race,
                        mInventory :: Inventory,
                        mLevel :: Int, 
                        mExperienceReward :: Int, 
                        mSpotted :: Bool,
                        
                        mID :: Int, -- to make each monster unique
                        
                        --common...                        
                        
                        eCurrHP :: Int,
                        eMaxHP :: Int,                        
                        
                        eSpeed :: Int, -- How much time between each action.
                        eNextMove :: Int, -- How much time until the NEXT action.                        
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        
                        eHitDie :: Dice,
                        eDamageDie :: Dice,
                        eEvadeDie :: Dice,
                        eMitigation :: Int,
                        eSkillEffects :: [SkillEffect]                        
                        
                      }
                  
            | Hero    { hName :: String,  
                        hClass :: Class,
                        hRace :: Race,
                        hInventory :: Inventory,
                        hLevel :: Int,
                        hExperienceRemaining :: Int,
                        hReputation :: Reputation,
                        hSkills :: [Skill],
                        
                        hCurrEnergy :: Int,
                        hMaxEnergy :: Int,
                        hWield :: Weapon,
                        hWear :: Armor,
                        hMovementSlack :: (Int, Int),  -- the coordinates that the hero can move between without wrapping.
                        hViewDistance :: Int, -- Added to $ snd hMovementSlack
                        
                        --common for all entities. Duplicated for ease of use.
                        eCurrHP :: Int,
                        eMaxHP :: Int,                        
                        
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        
                        eSpeed :: Int,
                        eNextMove :: Int,
                        
                        eHitDie :: Dice, -- updated on gear changes.
                        eDamageDie :: Dice,
                        eEvadeDie :: Dice,
                        eMitigation :: Int,
                        eSkillEffects :: [SkillEffect]
                        } 
            | Boss    { bName :: String,
                        bInnocentKills :: Int,
                        bRivalKills :: Int,
                        
                        eCurrHP :: Int,
                        eMaxHP :: Int,
                        
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        eSpeed :: Int,
                        eNextMove :: Int,
                        
                        eHitDie :: Dice,
                        eDamageDie :: Dice,
                        eEvadeDie :: Dice,
                        eMitigation :: Int,
                        eSkillEffects :: [SkillEffect]
                        
                        }
instance Eq Entity where
  x == y = case x of
    Hero {} -> case y of
      Hero {} -> True -- only one hero
      _ -> False
    Boss {} -> case y of
      Boss {} -> True -- only one boss
      _ -> False
    Monster {} -> case y of
      Monster {} -> (mID x) == (mID y)
      _ -> False
      


instance Show Entity where
    show e = filter (/= '\"') outString -- remove them silly "'s.
      where
        outString = case e of
          Hero {} -> (show $ hName e)
          Monster {} -> (show $ mRace e) ++ " " ++ (show $ mType e) ++ "[" ++ (show $ mLevel e) ++ "]"
          Boss {} -> show $ bName e
          
class ShowLong a where
  showLong  :: a -> String
          
instance ShowLong Entity where
  showLong e = filter (/= '\"') outString -- remove "
    where
      outString = case e of 
        Hero {} -> (show $ hName e) ++ " the " ++ (show $ hRace e) ++ " " ++ (show $ hClass e)
        _ -> "TODO: ShowLong Entity for bosses and monsters."
          
-------------------------------------          
          

-- Races:
-------------------------------------
data Race = Race { rName :: String, -- Has to be unique for each race (not enforced)
                   rHitModifier :: Int,
                   rEvasionModifier :: Int,
                   rDamageModifier :: Int,
                   rMitigationModifier :: Int,
                   
                   rBaseSpeed :: Int,
                   
                   rBaseHP :: Int,
                   rBaseHPPerLevel :: Int,
                   
                   rBaseEnergy :: Int,
                   rBaseEnergyPerLevel :: Int,
                   rBaseEnergyCost :: Int,
                                      
                   rExperiencePenalty :: Float,
                   
                   rContextFunc :: World -> World,
                   
                   rSkillMask :: [SkillMask]
                 }
instance Show Race where
  show r = show $ rName r

instance Eq Race where
  x == y = rName x == rName y
-------------------------------------  
  
  
-- Skills:
-------------------------------------

data SkillEffect = Final { seEffect :: Entity -> Entity, 
                           seDelay :: Int
                         } -- OneTime irreversible effect. Ex: instant damage
                          -- Optional: Delay x turns before applying effect.
                   
                 | Temporary { seEffect :: Entity -> Entity, -- Question, is it possible to reverse this only given the function? probably need 2 functions.
                               seReverseEffect :: Entity -> Entity,
                               seDuration :: Int,
                               seDelay :: Int
                             } -- Overtime reversible effect, ex: debuff.
                               -- Executed at start of druation, reversed at end of duration.
                               -- Optional: Delay x turns before applying effect.

                 | FinalOverTime { seEffect :: Entity -> Entity,
                                   seTimeBetweenTicks :: Int,
                                   seTickNumber :: Int,
                                   seDelay :: Int
                                 } -- Irreversible effect over time, Ex: hp loss over time.
                                   -- Optional: Delay x turns before applying effect.
                     
data SkillTarget = Self
                 | Other { stRange :: Int, 
                           stHitMask :: HitMask}
                 | Area  { stRange :: Int,
                           stRadius :: Int,
                           stHitMask :: HitMask
                         }
                 deriving(Eq)

                      




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
instance Eq Skill where
  x == y = (sName x) == (sName y)
-------------------------------------