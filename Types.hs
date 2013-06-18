module Types where

import qualified Data.Map as M
import Prelude hiding (Either(..))
type Position = (Int, Int)

data Villain = Villain { vCurrPos :: Position,
                         vGold :: Int,
                         vHP :: Int,
                         vItems :: [Item],
                         vOldPos :: Position }
data Item = Arm Armor | Pot Potion | Weap Weapon

data Armor = Armor { aDefence :: Int,
                     aDesc :: String }
             
data Potion = Potion { pAmount :: Int,
                       pDesc :: String,
                       pEffect :: Effect }
              
data Effect = Heal | Harm

data Weapon = Weapon { wDamage :: Int,
                       wDesc :: String,
                       wToHit :: Int }
data Tile = Water | Floor | Door -- | Stairs

                         
data Input = Dir Direction | Exit

data Direction = Up | Down | Left | Right

data Hero = Hero { hCurrPos :: Position,
                   hGold :: Int,
                   hHP :: Int,
                   hItems :: [Item],
                   hWields :: Weapon,
                   hWears :: Armor,
                   hOldPos :: Position,
                   hFacing :: Direction }

data Level = Level { lDepth :: Int,
                     lGold :: M.Map Position Int,
                     lItems :: M.Map Position Item,
                     lSize :: Int,
                     lTiles :: M.Map Position Tile,
                     lVillains :: M.Map Position Villain }

data World = World { wDepth :: Int,
                     wHero :: Hero,
                     wLevel :: Level,
                     wLevels :: [Level] }


emptyLevel = Level { lDepth = 0,
                     lGold = M.empty,
                     lItems = M.empty,
                     lSize = 1,
                     lTiles = M.empty,
                     lVillains = M.empty }

fists = Weapon 0 "Bare Fists" 0

rags = Armor 0 "Rags"

genesis = World { wDepth = 0,
                  wHero = commoner,
                  wLevel = emptyLevel,
                  wLevels = [emptyLevel] }

commoner = Hero { hCurrPos = (20,0),
                  hGold = 0,
                  hHP = 10,
                  hItems = [],
                  hOldPos = (1,1),
                  hWields = fists,
                  hWears = rags, 
                  hFacing = Right}