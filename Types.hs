module Types where

import qualified Data.Map as M
import Prelude hiding (Either(..))
import System.Random

type Position = (Int, Int)
type Gold = Int

calculateExperience :: Int -> Int -- Level -> experience required.
calculateExperience level = experience
  where
    experience = 100 * level;



data Entity = Monster { mType :: MonsterType,
                        mRace :: Race,
                        mInventory :: Inventory,
                        mLevel :: Int, 
                        mExperience :: Int, 
                        mCurrHP :: Int,
                        mMaxHP :: Int,
                        mID :: Int, -- to make each monster unique
                        
                        --common...                        
                        eSpeed :: Int, -- How much time between each action.
                        eNextMove :: Int, -- How much time until the NEXT action.                        
                        eCurrPos :: Position,
                        eOldPos :: Position
                      }
                  
            | Hero    { hName :: String,  
                        hClass :: Class,
                        hRace :: Race,
                        hInventory :: Inventory,
                        hLevel :: Int,
                        hExperienceRemaining :: Int,
                        hCurrHP :: Int,
                        hMaxHP :: Int,
                        hCurrEnergy :: Int,
                        hMaxEnergy :: Int,
                        hWield :: Weapon,
                        hWear :: Armor,
                        hMovementSlack :: (Int, Int),  -- the coordinates that the hero can move between without wrapping.
                        hViewDistance :: Int, -- Added to $ snd hMovementSlack
                        
                        --common for all entities. Duplicated for ease of use.
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        eSpeed :: Int,
                        eNextMove :: Int
                        } 
            | Boss    { bName :: String,
                        bInnocentKills :: Int,
                        bRivalKills :: Int,
                        bCurrHP :: Int,
                        bMaxHP :: Int,
                        
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        eSpeed :: Int,
                        eNextMove :: Int
                        }
            deriving (Show, Eq)

data Inventory = Inventory [Item] Gold
                 deriving (Show, Eq)


data Race = Ogre | Giant | Troll | Orc | Goblin | Hobgoblin
          deriving (Show, Bounded, Enum, Eq, Read)
               
data MonsterType = Politician | Noble 
                 deriving (Show, Bounded, Enum, Eq)
                 
instance Random MonsterType where
    random g = case randomR (fromEnum (minBound :: MonsterType), fromEnum (maxBound :: MonsterType)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')
                        

data Potion = Potion { pAmount :: Int,
                       pDesc :: String,
                       pEffect :: Effect } 
            deriving (Show, Eq)
             
data Effect = Heal | Harm
            deriving (Show, Eq)

data Item = Arm Armor | Pot Potion | Weap Weapon
          deriving (Show, Eq)

data Armor = Armor { aAvoidance :: Int,
                     aMitigation :: Int,
                     aDesc :: String }
           deriving(Show, Eq)


data Weapon = Weapon { wDamage :: Int,
                       wDesc :: String,
                       wToHit :: Int }
            deriving (Show, Eq)

data WallTile = Door | Wall
              deriving (Show, Bounded, Enum, Eq)

data FloorTile = Grass | Water 
               deriving (Show, Bounded, Enum, Eq)

instance Random FloorTile where
    random g = case randomR (fromEnum (minBound :: FloorTile), fromEnum (maxBound :: FloorTile)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

                         
data Input = Dir Direction | Exit | Wait | NoInput
           deriving (Eq, Show)

data Direction = Left | Right 
               deriving (Eq, Show)

                             
data Class = Bard | Jester | Fool
           deriving (Show, Eq, Read)

data Level = Level { lDepth :: Int,
                     lGold :: M.Map Position Int,
                     lItems :: M.Map Position [Item],
                     lSize :: Int,
                     lFloorTiles :: M.Map Position FloorTile,
                     lWallTiles :: M.Map Position WallTile,                     
                     lEntities :: M.Map Position [Entity] }
           deriving (Show)

data World = World { wDepth :: Int,
                     wHero :: Entity,
                     wLevel :: Level,
                     wLevels :: [Level], 
                     wPrevInput :: Input, 
                     wMessageBuffer :: [String],
                     wStdGen :: StdGen,
                     wBoss :: Entity
                   }
             
           deriving (Show)
                    
                    

data AttackType = Thump | Slash | Maul
                deriving (Show)



emptyLevel = Level { lDepth = 0,
                     lGold = M.empty,
                     lItems = M.empty,
                     lSize = 1,
                     lFloorTiles = M.empty,
                     lWallTiles = M.empty,                     
                     lEntities = M.empty }
             
             
baseMonster = Monster { mType = Noble,
                        mRace = Ogre,
                        mInventory = Inventory [] 0,
                        mLevel = 0, 
                        mExperience = 1, 
                        mID = 1,
                        eCurrPos = (0,0),
                        eOldPos = (0,0),
                        mCurrHP = 1,
                        mMaxHP = 1,
                        eSpeed = 10,
                        eNextMove = 10 }


fists = Weapon 0 "Bare Fists" 0

rags = Armor 0 0 "Rags"



genesis = World { wDepth = 0,
                  wHero = player,
                  wLevel = emptyLevel,
                  wLevels = [emptyLevel], 
                  wPrevInput = NoInput, 
                  wMessageBuffer = ["Welcome to Axis of X!"],
                  wStdGen = undefined,
                  wBoss = boss
                }


boss = Boss {  bName = "ROARWALD",
               bInnocentKills = 0,
               bRivalKills = 0,
               bCurrHP = 100,
               bMaxHP = 100,
               
               eCurrPos = (-200, 0),
               eOldPos = (-200, 0),
               eSpeed = 20,
               eNextMove = 20
               
            }

player = Hero { hName = "",
                hClass = Jester,
                hRace = Hobgoblin,
                hInventory = Inventory [] 0,
                hLevel = 1,
                hExperienceRemaining = 100,
                eCurrPos = (0,0),
                eOldPos = (0,0),
                hCurrHP = 10,
                hMaxHP = 10,
                hCurrEnergy = 50,
                hMaxEnergy = 50,
                eSpeed = 5,
                eNextMove = 0,
                hWield = fists,
                hWear = rags,
                hMovementSlack = (0, 9),
                hViewDistance = 15 }
                           