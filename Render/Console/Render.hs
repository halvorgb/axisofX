module Render.Console.Render (drawWorld, drawEntities) where

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
      h = wHero world
      minPoint = fst $ hMovementSlack h
      coord' = (fst coord - minPoint, snd coord) -- side scrolling


drawEntities world (posX, posY) = do
    mapM_ (drawCoord world) oldPositions
    mapM_ (drawCoord world) newPositions

    drawCoord world newHeroPos -- hack to test before combat and collision is implemented, LOL wut?
  where
    hero = wHero world
    entities = getEntitiesFromViewFrame world $ getViewFrame world
    newPositions = map alignInGUI $ map eCurrPos entities
    
    oldPositions = (map alignInGUI $ map eOldPos (hero:entities)) \\ newPositions -- remove the new to not draw redundant tiles.
    newHeroPos = alignInGUI $ eCurrPos hero
    
    alignInGUI (x,y) = (x+posX, y+posY)

    
    

drawWorld world p@(posX, posY) = do
  clearWorld (minPoint, maxPoint) world p
  setCursorPosition posY posX
  mapM_ drawChar (unlines chars)
    where
      (minPoint, maxPoint) = getViewFrame world
      
      chars = [[coordToChar (x,y) world | x <- [minPoint..maxPoint]]
                                        | y <- [0..1]] -- always 2


clearWorld (minPoint, maxPoint) world (posX, posY) = do
  setCursorPosition posY (maxPoint - minPoint      +posX + 1)
  mapM_ drawChar chars
  setCursorPosition (1+posY) (maxPoint - minPoint  +posX + 1)
  mapM_ drawChar chars
  where
    h = wHero world
    unBlockedMax = (snd $ hMovementSlack h) + hViewDistance h
    chars = if unBlockedMax > maxPoint then
              replicate (unBlockedMax - maxPoint) ' '
            else
              []
