module Render.SDL.Render (drawWorld, drawEntities, loadImages, TileSurfaces, TileType) where


import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi

import Level
import Types
import Logic

import qualified Data.Map as M
import Data.Maybe
import Data.List

gfxFilePaths = [ "assets/tiles/test_bg.png",
                 "assets/tiles/test_char.png",
                 "assets/tiles/test_door.png",
                 "assets/tiles/test_enemy.png",
                 "assets/tiles/test_grass.png",
                 "assets/tiles/test_water.png" ]
               
data TileType = TT_bg | TT_char | TT_door | TT_enemy | TT_grass | TT_water
               deriving (Bounded, Eq, Enum, Ord, Show)
                        
tileMaxBound :: Int
tileMaxBound = fromEnum (maxBound :: TileType)

tileTypes :: [TileType]
tileTypes = enumFrom TT_bg

type TileSurfaces = [(TileType, SDL.Surface)]


coordToTileType coord (World _ hero level _ )
  | eCurrPos hero == coord = TT_char
  | isWater coord level = TT_water
  | isDoor coord level = TT_door
  | isGrass coord level = TT_grass
  | isMonster coord level = TT_enemy
  | otherwise = TT_bg



loadGfx :: [String] -> IO TileSurfaces
loadGfx paths = do
  tileSurfaces <- mapM SDLi.load paths
  return $ zip tileTypes tileSurfaces
  
  
-- visible.
--loadImages :: IO TileSurfaces
loadImages = do
  loadGfx gfxFilePaths


--drawWorld :: World -> IO ()
drawWorld world mainSurface tileSurfaces = do
--  tileSurfaces <- loadGfx gfxFilePaths
  clearWorld (minPoint, maxPoint) world tileSurfaces mainSurface
  
  mapM_ (drawTile mainSurface tileSurfaces) wallTiles
  mapM_ (drawTile mainSurface tileSurfaces) floorTiles
  
--  return ()
    where
      (minPoint, maxPoint) = getViewFrame world
      xs = [0..(maxPoint-minPoint)]
      wallTiles = zip [(x,0) | x <- xs] [coordToTileType (x, 0) world | x <- [minPoint..maxPoint]]
      floorTiles = zip [(x,1) | x <- xs] [coordToTileType (x, 1) world | x <- [minPoint..maxPoint]]
  
--clearWorld :: (Int, Int) -> World -> TileSurfaces -> IO ()
clearWorld (minPoint, maxPoint) world tileSurfaces mainSurface = do
  mapM_ (drawTile mainSurface tileSurfaces)  wallTiles
  mapM_ (drawTile mainSurface tileSurfaces)  floorTiles
  -- 1return ()
    where
      h = wHero world
      unBlockedMax = (snd $ hMovementSlack h) + hViewDistance h
      tiles = if unBlockedMax > maxPoint then
                replicate (unBlockedMax - maxPoint) TT_bg
              else
                []
      xs = [(maxPoint-minPoint+1)..(unBlockedMax-minPoint+1)]
      wallTiles = zip [(x, 0) | x <- xs] tiles
      floorTiles = zip [(x, 1) | x <- xs] tiles
  

drawTile :: SDL.Surface -> TileSurfaces -> (Position, TileType) -> IO ()
drawTile destSurf ts ((x,y), tileType) = do
  let sourceRect = Just (SDL.Rect 0 0 32 32)
  let destRect   = Just (SDL.Rect  (32*x) (32*y) 0 0)  
  let sourceSurf = fromJust $ lookup tileType ts
  SDL.blitSurface sourceSurf sourceRect destSurf destRect  
  return ()
  
  
drawEntities :: IO ()
drawEntities = do
  return ()
  