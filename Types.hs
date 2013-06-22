module Types where

import qualified Data.Map as M
import Prelude hiding (Either(..))

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


               
data MonsterType = Politician | Noble deriving (Show, Eq)
               
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
data Tile = Water | Floor | Door
                         
data Input = Dir Direction | Exit

data Direction = Up | Down | Left | Right deriving (Eq)

                             
data Class = Bard | Jester

data Level = Level { lDepth :: Int,
                     lGold :: M.Map Position Int,
                     lItems :: M.Map Position [Item],
                     lSize :: Int,
                     lViewFrame :: (Int, Int),
                     lViewDistance :: Int,
                     lTiles :: M.Map Position Tile,
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
                     lTiles = M.empty,
                     lEntities = M.empty }

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