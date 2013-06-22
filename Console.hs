module Console where

import System.Console.ANSI
import Level 
import Types

import qualified Data.Map as M
import Data.Maybe

coordToChar coord (World _ hero level _)
  | eCurrPos hero == coord = '@'
  | isWater coord level = '~'
  | isDoor coord level = '|'
  | isFloor coord level = '#'
  | isMonster coord level = 'm'
  | isArmor coord level = 'a'
  | isWeapon coord level = 'w'
  | isGold  coord level  = '.'
  | isPotion coord level = 'p'
  | otherwise = ' '
    

tileColors = M.fromList [
  ('@', (Vivid, Blue)), 
  ('#', (Vivid, Black)), 
  ('~', (Vivid, Cyan)), 
  ('|', (Dull, Yellow)), 
  ('v', (Vivid, Red)), 
  ('a', (Dull, Green)), 
  ('w', (Dull, Green)), 
  ('.', (Dull, Yellow)), 
  ('p', (Vivid, Magenta)), 
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
      lvl = wLevel world
      minPoint = fst $ lViewFrame lvl
      coord' = (fst coord - minPoint, snd coord) -- side scrolling

drawHero world
  | newPos == oldPos = return ()
  | otherwise = do
    drawCoord world newPos
    drawCoord world oldPos
  where
    hero = wHero world
    newPos = eCurrPos hero
    oldPos = eOldPos hero

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
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
    where
      lvl = wLevel world
      (minPoint, maxPoint) = (fst $ lViewFrame lvl, (snd $ lViewFrame lvl) + lViewDistance lvl)
      chars = [[coordToChar (x,y) world | x <- [minPoint..maxPoint]]
                                        | y <- [0..1]] -- always 2!