module Main where

import Prelude hiding (Either(..))
import System.IO

import qualified Data.Map as M
import Data.Maybe
import Data.List


import Render.SDL.GUI as GUI


import Level
import Types
import Logic
import WorldBuilder

main = do
  world <- returnWorld
  GUI.setup world
  assets <- GUI.loadAssets  -- CALL #1

  gameLoop world assets     -- TO FUNC. gameLoop as PARAM


gameLoop world assets = do
  if (null $ wLevels world) -- check if complete. (future also check for death)
    then do
    GUI.shutdown
    
    else do
    if (eNextMove $ wHero world) == 0  -- check if hero's turn.
      then do 
      azz <- GUI.loadAssets -- ....
      GUI.update world azz -- THIS WORKS
      GUI.update world assets -- THIS NEVER WORKS.
      input <- GUI.getInput
      case input of 
        Exit -> handleExit
        Wait -> gameLoop (handleWait world) assets
        Dir dir -> gameLoop (handleDir world dir) assets
      else do -- else: AI
      world' <- think world    
      gameLoop world' assets


-- render the world relative to the player position to enable side scrolling. Only to the right.
handleDir w dir
  | (dir == Left)  && ((fst $ eCurrPos h) == firstInFrame) = 
    w -- left corner, does not use a turn (takes up a move, possible issue)
    
  | ((dir == Right) && ((fst $ coord) > (lSize lvl -1))) =  
      nextLevel w -- Right edge of map, does not use a turn, possible issue. Perhaps load next level here.


  | isDoor coord lvl = 
    w {
      wLevel = lvl { 
         lWallTiles = M.delete coord $ lWallTiles lvl
         },
      wHero = h { 
        eOldPos = eCurrPos h, 
        eNextMove = eSpeed h
        } 
      } -- Destroys the door, uses a turn.
    
  | (dir == Right) && ((fst $ eCurrPos h) == lastInFrame) && (lastInFrame < lSize lvl) = 
      w { 
        wHero = h { 
           eOldPos = eCurrPos h,
           eCurrPos = coord, 
           eNextMove = eSpeed h,
           hMovementSlack = (firstInFrame+1, lastInFrame+1)
           }
        } -- If at right side of frame -- TODO: check if monster collides! (above)
      
  | otherwise =  
        w { wHero = h { eOldPos = eCurrPos h, eCurrPos = coord, eNextMove = eSpeed h} }
  where 
    h              = wHero w
    lvl            = wLevel w
    coord          = (newX, newY)
    newX           = heroX
    newY           = 0
    (heroX, heroY) = eCurrPos h |+| dirToCoord dir
    hConst i       = max 0 (min i 20)
    (firstInFrame, lastInFrame) = hMovementSlack h


--handleWait :: World -> World
handleWait w = 
  w {
    wHero = h { 
       eOldPos = eCurrPos h,
       eNextMove = eSpeed h
       } 
}
  where
    h = wHero w

handleExit = do
  GUI.shutdown
  
  
--nextLevel :: World -> IO 
nextLevel world
  | null $ tail $ wLevels world =
    world { wLevels = [] }  -- just set wLevels to [] and check for that in the main Loop... (ugly.)
  | otherwise = 
      world { wLevel =  (wLevels world) !! 1, 
              wLevels = tail $ wLevels world, 
              wHero = hero { eCurrPos = (0,0),
                             eOldPos = (0,0), 
                             hMovementSlack = (0, 9)}
            }
  where
    hero = wHero world
    