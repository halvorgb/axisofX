module AxisData.World where



import qualified Data.Map as M
import System.Random


import AxisData.Classes
import AxisData.Common
import AxisData.Entities
import AxisData.Items.Armor
import AxisData.Items.Inventory
import AxisData.Items.Weapons
import AxisData.Tiles

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
                    