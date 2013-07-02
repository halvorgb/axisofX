module Types where

import qualified Data.Map as M
import Prelude hiding (Either(..))
import System.Random

type Position = (Int, Int)
type Gold = Int

data EntityType = Monster { mType :: MonsterType,
                            mInventory :: Inventory,
                            mLevel :: Int }
                  
                | Hero    { hGold :: Int,
                            hClass :: Class,
                            hLevel :: Int,
                            hExperience :: Int,
                            hHP :: Int,
                            hItems :: [Item],
                            hWields :: Weapon,
                            hWears :: Armor  }
                  
                | Object  { oDamage :: Int, 
                            oVelocity :: Int} -- currently only projectiles ?

data Entity = Entity { eCurrPos :: Position,
                       eOldPos :: Position,
                       eEntityType :: EntityType,
                       eSpeed :: Int,
                       eTimeUntilNextMove :: Int }
                
                

data Inventory = Inventory [Item] Gold


               
data MonsterType = Politician | Noble 
                 deriving (Show, Bounded, Enum, Eq)
                 

                          
instance Random MonsterType where
    random g = case randomR (fromEnum (minBound :: MonsterType), fromEnum (maxBound :: MonsterType)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')
                          





               
data Item = Arm Armor | Pot Potion | Weap Weapon

data Armor = Armor { aAvoidance :: Int,
                     aMitigation :: Int,
                     aDesc :: String }
             
data Potion = Potion { pAmount :: Int,
                       pDesc :: String,
                       pEffect :: Effect }
              
data Effect = Heal | Harm

data Weapon = Weapon { wDamage :: Int,
                       wDesc :: String,
                       wToHit :: Int }
--data Wall = Door deriving (Eq, Ord)

data WallTile = Wall 
              deriving (Show, Bounded, Enum, Eq)

data FloorTile = Grass | Water 
               deriving (Show, Bounded, Enum, Eq)

instance Random FloorTile where
    random g = case randomR (fromEnum (minBound :: FloorTile), fromEnum (maxBound :: FloorTile)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

instance Random WallTile where
    random g = case randomR (fromEnum (minBound :: WallTile), fromEnum (maxBound :: WallTile)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')


{-
instance Random Tile where 
  randomR = randomTileRange
  random = randomTile

randomTileRange :: (RandomGen g, Tile t) => (t, t) -> g -> (t, g)
-}

                         
data Input = Dir Direction | Exit

data Direction = Up | Down | Left | Right deriving (Eq)

                             
data Class = Bard | Jester

data Level = Level { lDepth :: Int,
                     lGold :: M.Map Position Int,
                     lItems :: M.Map Position [Item],
                     lSize :: Int,
                     lViewFrame :: (Int, Int),
                     lViewDistance :: Int,
                     lFloorTiles :: M.Map Position FloorTile,
                     lWallTiles :: M.Map Position WallTile,                     
                     lEntities :: M.Map Position [Entity] }

data World = World { wDepth :: Int,
                     wHero :: Entity,
                     wLevel :: Level,
                     wLevels :: [Level] }


emptyLevel = Level { lDepth = 0,
                     lGold = M.empty,
                     lItems = M.empty,
                     lSize = 1,
                     lViewFrame = (0, 9),
                     lViewDistance = 30,
                     lFloorTiles = M.empty,
                     lWallTiles = M.empty,                     
                     lEntities = M.empty }
             
             
baseMonster = Monster { mType = Noble,
                        mInventory = Inventory [] 0,
                        mLevel = 0 }
              
baseMonsterEnt = Entity { eCurrPos = (0,0),
                          eOldPos = (0,0),
                          eEntityType = baseMonster,
                          eSpeed = 10,
                          eTimeUntilNextMove = 10 }

fists = Weapon 0 "Bare Fists" 0

rags = Armor 0 0 "Rags"


genesis = World { wDepth = 0,
                  wHero = commonerEnt,
                  wLevel = emptyLevel,
                  wLevels = [emptyLevel] }


commonerHero =  Hero { hGold = 0,
                       hClass = Bard,
                       hLevel = 0,
                       hExperience = 0,
                       hHP = 10,
                       hItems = [],
                       hWields = fists,
                       hWears = rags  }
                
commonerEnt = Entity  { eCurrPos = (0,0),
                        eOldPos = (0,0),
                        eSpeed = 5,
                        eEntityType = commonerHero,
                        eTimeUntilNextMove = 0 }