module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

import qualified Data.Map as M
import Data.Maybe
import Data.List

import Console
import Level
import Types
import Logic
import WorldBuilder

main = do
  setSGR [ Reset ]
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Axis of X"
  clearScreen
  world <- returnWorld
  gameLoop $ world

gameLoop world = do -- entities are the hero, any projectiles and any monsters.
  if (eNextMove $ wHero world) == 0 
    then do 
    drawWorld world
    drawEntities world
    input <- getInput 
    case input of
      Exit -> handleExit
      Wait -> handleWait world
      Dir dir -> handleDir world dir
    else do
    world' <- think world    
    gameLoop world'


getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'a' -> return (Dir Left)
    's' -> return Wait
    'd' -> return (Dir Right)
    _   -> getInput -- recurse if input not recognized



-- render the world relative to the player position to enable side scrolling. Only to the right.
handleDir w dir
  | (dir == Left)  && ((fst $ eCurrPos h) == firstInFrame) = 
    gameLoop w -- left corner, does not use a turn (takes up a move, possible issue)
    
  | ((dir == Right) && ((fst $ coord) > (lSize lvl -1))) =  
      nextLevel w -- Right edge of map, does not use a turn, possible issue. Perhaps load next level here.


  | isDoor coord lvl = 
    gameLoop w {
      wLevel = lvl { 
         lWallTiles = M.delete coord $ lWallTiles lvl
         },
      wHero = h { 
        eOldPos = eCurrPos h, 
        eNextMove = eSpeed h
        } 
      } -- Destroys the door, uses a turn.

  | (dir == Right) && ((fst $ eCurrPos h) == lastInFrame) && (lastInFrame < lSize lvl) = 
      gameLoop w { 
        wHero = h { 
           eOldPos = eCurrPos h,
           eCurrPos = coord, 
           eNextMove = eSpeed h,
           hMovementSlack = (firstInFrame+1, lastInFrame+1)
           }
        } -- If at right side of frame -- TODO: check if monster collides! (above)
      
  | otherwise =  
        gameLoop w { wHero = h { eOldPos = eCurrPos h, eCurrPos = coord, eNextMove = eSpeed h} }
  where 
    h              = wHero w
    lvl            = wLevel w
    coord          = (newX, newY)
    newX           = heroX
    newY           = 0
    (heroX, heroY) = eCurrPos h |+| dirToCoord dir
    hConst i       = max 0 (min i 20)
    (firstInFrame, lastInFrame) = hMovementSlack h



handleWait w =
  gameLoop w { 
    wHero = h { 
       eOldPos = eCurrPos h,
       eNextMove = eSpeed h
       } 
    }
  where
    h = wHero w

handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ Reset ]
  putStrLn "Thank you for playing Axis of X!"
  
  
nextLevel :: World -> IO ()
nextLevel world = do
  if null $ tail $ wLevels world
    then do
    clearScreen
    setCursorPosition 0 0
    showCursor
    setSGR [ Reset ]
    putStrLn "Victory!"
    else do
    let hero = wHero world in
      gameLoop world { wLevel =  (wLevels world) !! 1, 
                       wLevels = tail $ wLevels world, 
                       wHero = hero { eCurrPos = (0,0),
                                      eOldPos = (0,0), 
                                      hMovementSlack = (0, 9)}
                     }
    