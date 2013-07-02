module Console where

import System.Console.ANSI
import Level 
import Types
import Logic

import qualified Data.Map as M
import Data.Maybe
import Data.List

coordToChar coord (World _ hero level _)
  | eCurrPos hero == coord = '@'
  | isWater coord level = '~'
  | isDoor coord level = '|'
  | isGrass coord level = '#'
  | isMonster coord level = 'm'
  | isArmor coord level = '-'  
  | isWeapon coord level = ','
  | isGold  coord level  = '.'
  | isPotion coord level = '+'
  | otherwise = ' '
    

tileColors = M.fromList [
  ('@', (Vivid, Blue)), 
  ('#', (Vivid, Green)), 
  ('~', (Vivid, Cyan)), 
  ('|', (Dull, Yellow)), 
  ('m', (Vivid, Red)), 
  ('-', (Dull, White)), 
  (',', (Dull, White)), 
  ('.', (Dull, Yellow)), 
  ('+', (Vivid, Magenta)), 
  (' ', (Vivid, Black))  ]

drawChar :: Char -> IO ()
drawChar c = do
  setSGR [SetConsoleIntensity BoldIntensity,
          SetColor Background Dull Black,
          SetColor Foreground (fst tileColor) (snd tileColor)]
  putChar c
  where
    tileColor = fromMaybe (Vivid, Black) $ M.lookup c tileColors


drawCoord world coord = do
  uncurry (flip setCursorPosition) coord'
  drawChar (coordToChar coord world)
    where
      ht = eEntityType $ wHero world
      minPoint = fst $ hViewFrame ht
      coord' = (fst coord - minPoint, snd coord) -- side scrolling


drawEntities world = do
    --mapM_ (drawCoord world) oldPositions
    mapM_ (drawCoord world) newPositions

    drawCoord world newHeroPos -- hack to test before combat and collision is implemented, LOL wut?
  where
    hero = wHero world
    entities = getEntitiesFromViewFrame world $ getViewFrame world
    newPositions = map eCurrPos entities
    oldPositions = (map eOldPos (hero:entities)) \\ newPositions -- remove the new to not draw redundant tiles.
    newHeroPos = eCurrPos hero

    
    

drawWorld world = do
  clearWorld (minPoint, maxPoint) world
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
    where
      (minPoint, maxPoint) = getViewFrame world
      
      chars = [[coordToChar (x,y) world | x <- [minPoint..maxPoint]]
                                        | y <- [0..1]] -- always 2!


clearWorld (minPoint, maxPoint) world = do
  setCursorPosition 0 (maxPoint - minPoint)
  mapM_ drawChar chars
  setCursorPosition 1 (maxPoint - minPoint)
  mapM_ drawChar chars
  where
    ht = eEntityType $ wHero world
    
    unBlockedMax = (snd $ hViewFrame ht) + hViewDistance ht
    chars = if unBlockedMax > maxPoint then
              replicate (unBlockedMax - maxPoint + 1) ' '
            else
              []
