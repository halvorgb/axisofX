module Render.SDL.Render (drawWorld, loadImages, ImageAssets) where


import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi
import Graphics.UI.SDL.TTF  as SDLttf

import Helpers
import Logic

import Types.Common
import Types.World
import Types.Tiles


import qualified Data.Map as M
import Data.Maybe
import Data.List

tileFilePaths = [ "assets/tiles/test_bg.png",
                 "assets/tiles/test_char.png",
                 "assets/tiles/test_door.png",
                 "assets/tiles/test_enemy.png",
                 "assets/tiles/test_grass.png",
                 "assets/tiles/test_water.png" ]
                
bgFilePath = "assets/background/gui_bg.png"
splashFilePath = "assets/background/splash_bg.png"


               
data TileType = TT_bg | TT_char | TT_door | TT_enemy | TT_grass | TT_water
               deriving (Bounded, Eq, Enum, Ord, Show)
                        
tileMaxBound :: Int
tileMaxBound = fromEnum (maxBound :: TileType)

tileTypes :: [TileType]
tileTypes = enumFrom TT_bg


type TileSurfaces = [(TileType, SDL.Surface)]

type Backgrounds = (SDL.Surface, SDL.Surface)

type ImageAssets  = (Backgrounds, TileSurfaces)

coordToTileType coord world 
  | eCurrPos hero == coord = TT_char
  | isWater coord level = TT_water
  | isDoor coord level = TT_door
  | isGrass coord level = TT_grass
  | isMonster coord level = TT_enemy
  | otherwise = TT_bg
  where
    hero = wHero world
    level = wLevel world



loadTiles :: [String] -> IO TileSurfaces
loadTiles paths = do
  tileSurfaces <- mapM SDLi.load paths
  return $ zip tileTypes tileSurfaces
  
-- visible.
loadImages :: IO ImageAssets
loadImages = do
  bgSurf <- SDLi.load bgFilePath
  splashSurf <- SDLi.load splashFilePath
  tileSurfs <- loadTiles tileFilePaths
  return ((splashSurf, bgSurf), tileSurfs)
  


drawWorld :: World -> SDL.Surface -> TileSurfaces -> IO ()
drawWorld world mainSurface tileSurfaces = do
  clearWorld  tileSurfaces mainSurface
  
  mapM_ (drawTile mainSurface tileSurfaces) wallTiles
  mapM_ (drawTile mainSurface tileSurfaces) floorTiles

  
    where
      (minPoint, maxPoint) = getViewFrame world
      xs = [0..(maxPoint-minPoint)]
      wallTiles = zip [(x,0) | x <- xs] [coordToTileType (x, 0) world | x <- [minPoint..maxPoint]]
      floorTiles = zip [(x,1) | x <- xs] [coordToTileType (x, 1) world | x <- [minPoint..maxPoint]]
  
--clearWorld :: (Int, Int) -> World -> TileSurfaces -> IO ()
clearWorld tileSurfaces mainSurface = do
  mapM_ (drawTile mainSurface tileSurfaces)  wallTiles
  mapM_ (drawTile mainSurface tileSurfaces)  floorTiles
    where
      tiles = replicate (div 800 16) TT_bg
      
      xs = [0..]
      wallTiles = zip [(x, 0) | x <- xs] tiles
      floorTiles = zip [(x, 1) | x <- xs] tiles
      
drawTile :: SDL.Surface -> TileSurfaces -> (Position, TileType) -> IO ()
drawTile destSurf ts ((x,y), tileType) = do
  let sourceRect = Just (SDL.Rect 0 0 32 32)
  let destRect   = Just (SDL.Rect  (32*x) (32*y) 0 0)  
  let sourceSurf = fromJust $ lookup tileType ts
  SDL.blitSurface sourceSurf sourceRect destSurf destRect  
  return ()
  
  
  