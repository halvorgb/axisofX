module Types where

import qualified Data.Map as M
import Prelude hiding (Either(..))

type Position = (Int, Int)
type Gold = Int


data Inventory = Inventory [Item] Gold

data Monster = Monster { mCurrPos :: Position,
                         mOldPos :: Position,
                         mType :: MonsterType,
                         mInventory :: Inventory,
                         mLevel :: Int }
               
data MonsterType = Politician | Noble
               
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

data Hero = Hero { hCurrPos :: Position,
                   hGold :: Int,
                   hClass :: Class,
                   hLevel :: Int,
                   hExperience :: Int,
                   hHP :: Int,
                   hItems :: [Item],
                   hWields :: Weapon,
                   hWears :: Armor,
                   hOldPos :: Position }
            
data Class = Bard | Jester

data Level = Level { lDepth :: Int,
                     lGold :: M.Map Position Int,
                     lItems :: M.Map Position [Item],
                     lSize :: Int,
                     lViewFrame :: (Int, Int),
                     lViewDistance :: Int,
                     lTiles :: M.Map Position Tile,
                     lMonsters :: M.Map Position Monster }

data World = World { wDepth :: Int,
                     wHero :: Hero,
                     wLevel :: Level,
                     wLevels :: [Level] }


emptyLevel = Level { lDepth = 0,
                     lGold = M.empty,
                     lItems = M.empty,
                     lSize = 1,
                     lViewFrame = (0, 9),
                     lViewDistance = 30,
                     lTiles = M.empty,
                     lMonsters = M.empty }

fists = Weapon 0 "Bare Fists" 0

rags = Armor 0 0 "Rags"


genesis = World { wDepth = 0,
                  wHero = commoner,
                  wLevel = emptyLevel,
                  wLevels = [emptyLevel] }

commoner = Hero { hCurrPos = (0,0),
                  hGold = 0,
                  hClass = Bard,
                  hLevel = 0,
                  hExperience = 0,
                  hHP = 10,
                  hItems = [],
                  hOldPos = (0,0),
                  hWields = fists,
                  hWears = rags }