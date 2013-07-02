module Console where

import System.Console.ANSI
import Level 
import Types
import Logic

import qualified Data.Map as M
import Data.Maybe

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
  ('v', (Vivid, Red)), 
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
    isMapped = M.lookup c tileColors
    tileColor
      | isMapped == Nothing = (Vivid, Black)
      | otherwise = fromJust isMapped

drawCoord world coord = do
  uncurry (flip setCursorPosition) coord'
  drawChar (coordToChar coord world)
    where
      ht = eEntityType $ wHero world
      minPoint = fst $ hViewFrame ht
      coord' = (fst coord - minPoint, snd coord) -- side scrolling


drawEntities world
  | newPos == oldPos = return ()
  | otherwise = do
    drawCoord world newPos
    drawCoord world oldPos
  where
    hero = wHero world
    newPos = eCurrPos hero
    oldPos = eOldPos hero

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
