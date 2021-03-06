
module Types.World where

import qualified Data.Map as M
import System.Random

import Types.Common
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
                     wScreenShown :: Screen,
                     wTimeElapsed :: Integer
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
                        mBehaviorStack :: [World -> Entity -> Maybe World],
                        
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
                        
                        hSkillQueue :: SkillQueue,
                        
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
      Monster {} -> mID x == mID y
      _ -> False
      


instance Show Entity where
    show e = filter (/= '\"') outString -- remove them silly "'s.
      where
        outString = case e of
          Hero {} -> show $ hName e
          Monster {} -> show (mRace e) ++ " " ++ show (mType e)
          Boss {} -> show $ bName e
          
instance ShowLong Entity where
  showLong e = filter (/= '\"') outString -- remove "
    where
      outString = case e of 
        Hero {} -> show (hName e) ++ " the " ++ show (hRace e) ++ " " ++ show (hClass e)
        Monster {} -> show e ++ " LVL:" ++ show (mLevel e) ++ " HP:" ++ show (eCurrHP e) ++ "/" ++ show (eMaxHP e)
        _ -> "TODO: ShowLong Entity for bosses ."
          
-------------------------------------          
          

-- Races:
-------------------------------------
data Race = Race { rName :: String, -- Has to be unique for each race (not enforced)
                   rHitModifier :: Int,
                   rEvasionModifier :: Int,
                   rDamageModifier :: Int,
                   rMitigationModifier :: Int,
                   
                   rSpeedMultiplier :: Float,
                   rHPMultiplier :: Float,
                   rEnergyMultiplier :: Float,
                                      
                   rExperienceMultiplier :: Float,
                   
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
data SkillResult = MISS | SUCC | MIT | BUFF | FAT | FAIL FailureCode
                 deriving (Eq, Show)
                          
data FailureCode = NoTarget | CantReach
                 deriving (Eq, Show)
  
  
class QuadrupleSkillQueue q where
  first :: q -> Skill
  second :: q -> Skill
  third :: q -> Skill
  fourth :: q -> Skill
  
  addToQueue :: Skill -> Int -> q -> q
  removeFromQueue :: Int -> q -> q
  clearQueue :: q
  
  toList :: q -> [Skill]
  
data SkillQueue = SkillQueue Skill Skill Skill Skill

instance QuadrupleSkillQueue SkillQueue where
  first  (SkillQueue s _ _ _) = s
  second (SkillQueue _ s _ _) = s
  third  (SkillQueue _ _ s _) = s
  fourth (SkillQueue _ _ _ s) = s
  
  addToQueue s i (SkillQueue fs sn th fo) = case i of
    1 -> SkillQueue s sn th fo
    2 -> case fs of
      NoSkill -> SkillQueue s sn th fo
      _ -> SkillQueue fs s th fo
    3 -> case fs of
      NoSkill -> SkillQueue s sn th fo
      _ -> case sn of
        NoSkill -> SkillQueue fs s th fo
        _ -> SkillQueue fs sn s fo
    4 -> case fs of
      NoSkill -> SkillQueue s sn th fo
      _ -> case sn of
        NoSkill -> SkillQueue fs s th fo
        _ -> case th of
          NoSkill -> SkillQueue fs sn s fo
          _ -> SkillQueue fs sn th s
    _ -> error "addToQueue index out of bounds."
    
  removeFromQueue i (SkillQueue fs sn th fo) = case i of
    1 -> SkillQueue NoSkill NoSkill NoSkill NoSkill
    2 -> SkillQueue fs NoSkill NoSkill NoSkill
    3 -> SkillQueue fs sn NoSkill NoSkill
    4 -> SkillQueue fs sn th NoSkill  
    _ -> error "removeFromQueue index out of bounds"

    
  clearQueue = SkillQueue NoSkill NoSkill NoSkill NoSkill
   
  toList (SkillQueue fs sn th fo) =
    case fs of 
      NoSkill -> []
      _ -> case sn of
        NoSkill -> [fs]
        _ -> case th of
          NoSkill -> [fs, sn]
          _ -> case fo of
            NoSkill -> [fs, sn, th]
            _ -> [fs, sn, th, fo]
  
  

data SkillEffect = FinalConstant { seFunc :: SkillEffectFunction
                                 } -- OneTime irreversible effect. Ex: instant damage
                                   -- Optional: Delay x turns before applying effect.
                 | FinalScaling  { seFunc :: SkillEffectFunction,
                                   seScale :: Float
                                 }
                   
                 | Temporary     { seFunc :: SkillEffectFunction,
                                   seValue :: Int,
                                   seDuration :: Int
                             } -- Overtime reversible effect, ex: debuff.
                               -- Executed at start of druation, reversed at end of duration.
                               -- Optional: Delay x turns before applying effect.

                 | FinalOverTime { seFunc :: SkillEffectFunction,
                                   seValue :: Int,
                                   seTimeBetweenTicks :: Int,
                                   seTickNumber :: Int
                                 } -- Irreversible effect over time, Ex: hp loss over time.
                                   -- Optional: Delay x turns before applying effect.

                           -- s,     hit,   evd,  dmg,    mit,  destEnt,   oldWorld, newWolrd
type SkillEffectFunction = (Skill ->  Int -> Int -> Int -> Int -> Entity -> World -> World)                   
type SkillTarget  = (World -> [Entity])


data Skill = Active { sName :: String,
                      sShortName :: String,
                      sDescription :: String,
                      sEffect :: [SkillEffect], -- allow multiple effects?
                      sTarget :: SkillTarget,  -- ALWAYS have a target, if in doubt put hero as target.
                      sPrequisites :: [Skill],
                      sSkillMask :: [SkillMask],
                      sWeaponConstraints :: WeaponConstraints,
                      sHitMask :: HitMask,
                      
                      sEnergyCost :: Int,
                      sSpeedMultiplier :: Float,
                      
                      sCoolDown :: Int
                     -- MORE
                      
                     }
             
             -- TODO: Sustained.
           | Sustained { sName :: String,
                         sShortName :: String,
                         sDescription :: String,
                         sEffect :: [SkillEffect],
                         sTarget :: SkillTarget,
                         sPrequisites :: [Skill],
                         sSkillMask :: [SkillMask],
                         sWeaponConstraints :: WeaponConstraints,
                         sHitMask :: HitMask,
                         
                         sEnergyUpkeep :: Int
                         
                         -- MORE
                       }
           | NoSkill
             
instance Eq Skill where
  x == y = case x of
    NoSkill -> case y of 
      NoSkill -> True
      _ -> False
    _ -> sName x == sName y
    
instance Show Skill where
  show x = case x of
    NoSkill -> "-"
    _       -> filter (/= '\"') $ show $ sShortName x
    
instance ShowLong Skill where
  showLong x = filter (/= '\"')  (show (sName x) ++ " (" ++ show (sShortName x) ++ "): " ++ show (sDescription x))
-------------------------------------

-- Classes:
-------------------------------------  
-- work on this, main idea: the player is a dick by default.
 -- Best reputation: Dick or asshole? Selfish?
data Reputation = Malevolent | Malicous | Hard 
                | Sociopath -- Neutral lol
                | Asshole
              deriving (Eq, Show)


data Class = 
  Class { cName :: String,
          cExpReq :: Int, 
          cBaseSpeed :: Int,
          cBaseHP :: Int,
          cHPPerLevel :: Int,
          
          cBaseEnergy :: Int,
          cEnergyPerLevel :: Int,
          
          cStartingWeapon :: Weapon,
          cStartingArmor :: Armor,
          cStartingInventory :: Inventory,
          cStartingSkills :: [Skill],
          cStartingReputation :: Reputation,
          
          cHitDie :: Dice,
          cEvadeDie :: Dice,
          cDamageBonus :: Int,
          cMitigationBonus :: Int,
          
          cSkillMask :: [SkillMask],
          cWeaponConstraints :: WeaponConstraints
          -- more attributes such as allowed weapons and skills.
        }
  deriving (Eq)

instance Show Class where
  show c = filter (/= '\"') $ show $ cName c
-------------------------------------

-- MonsterTypes:
-------------------------------------
data MonsterType =
  MonsterType { mtName :: String,
                mtExpReward :: Int,
                
                mtBaseHP :: Int,
                mtHPPerLevel :: Int,
                
                mtBaseSpeed :: Int,
--                mtSkills = [Skills],
                -- levelConstraints :: (Int, Int),                
                -- RaceConstraints?
                
                mtBehaviorStack :: [World -> Entity -> Maybe World],
                
                mtHitDie :: Dice,
                mtEvadeDie :: Dice,
                mtDamageDie :: Dice,
                mtMitigation :: Int
              }

instance Show MonsterType where
  show mt = show $ mtName mt
instance Eq MonsterType where
  x == y = mtName x == mtName y
-------------------------------------