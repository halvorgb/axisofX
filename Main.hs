module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

import Console
import Level
import Types

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Axis of X"
  clearScreen
  let world = genesis { wLevel = level1, wLevels = [level1] }
  drawWorld world
  gameLoop $ world

gameLoop world = do
  drawHero world
  input <- getInput
  case input of
    Exit -> handleExit
    Dir dir -> handleDir world dir

getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'a' -> return (Dir Left)
    'd' -> return (Dir Right)
    _   -> getInput -- recurse if input not recognized
    
handleDir w dir
  | isDoor coord lvl = gameLoop w { wHero = h { hOldPos = hCurrPos h, hFacing = dir} }
  | otherwise =  gameLoop w { wHero = h { hOldPos = hCurrPos h, hCurrPos = coord, hFacing = dir} }
  where 
    h              = wHero w
    lvl            = wLevel w
    facing         = dir
    coord          = (newX, newY)
    newX           = hConst heroX
    newY           = 0
    (heroX, heroY) = hCurrPos h |+| dirToCoord dir
    hConst i       = max 20 (min i 60)

-- just getting a prototype running...
dirToCoord Up    = (0, -1)
dirToCoord Down  = (0,  1)
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)

(|+|) :: Position -> Position -> Position
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ Reset ]
  putStrLn "Thank you for playing Axis of X!"
  
