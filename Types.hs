module Types where

import qualified Data.Map as M
import Prelude hiding (Either(..))
import System.Random

type Position = (Int, Int)
type Gold = Int

data Entity = Monster { mType :: MonsterType,
                        mRace :: Race,
                        mInventory :: Inventory,
                        mLevel :: Int, 
                        mExperience :: Int, 
                        mCurrPos :: Position,
                        mOldPos :: Position,
                        mCurrHP :: Int,
                        mMaxHP :: Int,
                        mSpeed :: Int, -- How much time between each action.
                        mNextMove :: Int -- How much time until the NEXT action.                                               
                      }
                  
            | Hero    { hClass :: Class,
                        hRace :: Race,
                        hInventory :: Inventory,
                        hLevel :: Int,
                        hExperience :: Int,
                        hExperiencePenalty :: Float,
                        hCurrPos :: Position,
                        hOldPos :: Position,
                        hCurrHP :: Int,
                        hMaxHP :: Int,
                        hSpeed :: Int,
                        hNextMove :: Int,
                        hWield :: Weapon,
                        hWear :: Armor,
                        hMovementSlack :: (Int, Int),  -- the coordinates that the hero can move between without wrapping.
                        hViewDistance :: Int } -- Added to $ snd hMovementSlack
                                          
            | Projectile  { pDamage :: Int,
                            pCurrPos :: Position,
                            pOldPos :: Position,
                            pSpeed :: Int,
                            pNextMove :: Int }
            deriving (Show)

data Inventory = Inventory [Item] Gold
                 deriving (Show)


data Race = Ogre | Giant | Troll | Orc | Goblin | Hobgoblin
          deriving (Show, Bounded, Enum, Eq)
               
data MonsterType = Politician | Noble 
                 deriving (Show, Bounded, Enum, Eq)
                 
instance Random MonsterType where
    random g = case randomR (fromEnum (minBound :: MonsterType), fromEnum (maxBound :: MonsterType)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')
                          
             


data Item = Arm Armor | Pot Potion | Weap Weapon
          deriving (Show, Eq)

data Armor = Armor { aAvoidance :: Int,
                     aMitigation :: Int,
                     aDesc :: String }
           deriving(Show, Eq)
             
data Potion = Potion { pAmount :: Int,
                       pDesc :: String,
                       pEffect :: Effect } 
            deriving (Show, Eq)
             
data Effect = Heal | Harm
            deriving (Show, Eq)

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

                         
data Input = Dir Direction | Exit | Wait

data Direction = Left | Right deriving (Eq)

                             
data Class = Bard | Jester
           deriving (Show, Eq)

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
                     wLevels :: [Level] }
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
                        mCurrPos = (0,0),
                        mOldPos = (0,0),
                        mCurrHP = 1,
                        mMaxHP = 1,
                        mSpeed = 10,
                        mNextMove = 10 }


fists = Weapon 0 "Bare Fists" 0

rags = Armor 0 0 "Rags"


genesis = World { wDepth = 0,
                  wHero = player,
                  wLevel = emptyLevel,
                  wLevels = [emptyLevel] }




player = Hero { hClass = Jester,
                hRace = Hobgoblin,
                hInventory = Inventory [] 0,
                hLevel = 1,
                hExperience = 0,
                hExperiencePenalty = 0.0,
                hCurrPos = (0,0),
                hOldPos = (0,0),
                hCurrHP = 10,
                hMaxHP = 10,
                hSpeed = 5,
                hNextMove = 0,
                hWield = fists,
                hWear = rags,
                hMovementSlack = (0, 9),
                hViewDistance = 40 }
                           