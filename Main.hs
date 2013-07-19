module Main where

import Prelude hiding (Either(..))
import System.IO
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Render.SDL.GUI as GUI
import Render.SDL.Render
import Combat
import Helpers
import Logic
import WorldBuilder

import Types.World
import Types.Common


import Content.Classes
import Content.Races
import Content.Skills



main :: IO ()
main = do
  assets <- loadAssets
  GUI.setup
  world <- returnWorld
  
  (name, race, klass) <- GUI.chooseProtagonist world assets
  let world' = createHero world  name klass race

  
  gameLoop world' assets

-- main game loop!
gameLoop :: World -> Assets -> IO ()
gameLoop world assets = do
  if (null $ wLevels world) || ((eCurrHP $ wHero world) < 0) -- check if complete. (future also check for death)
    then do
    GUI.delayedShutdown world assets
    else do
    if (eNextMove $ wHero world) == 0  -- check if hero's turn.
      then do 
      let world' = checkVision world
      GUI.update_ world' assets
      input <- GUI.getInput
      case input of 
        Show screen -> gameLoop (handleShow screen world') assets
        Exit -> handleExit (world' {wScreenShown = Console}) assets
        Wait -> gameLoop (handleWait (world' {wScreenShown = Console})) assets
        Dir dir -> gameLoop (handleDir (world' {wScreenShown = Console}) dir) assets
        Queue i -> gameLoop (world' {wScreenShown = Skills}) assets
        ExecuteSkills -> gameLoop (world' {wScreenShown = Console}) assets
      else do -- else: AI
      world' <- think world
      gameLoop world' assets


-- tries to move the player.
      -- if a door is encountered, open it.
      -- if an enemy is encountered: attack it,
handleDir :: World -> Direction -> World
handleDir w dir
  | (dir == Left) && ((fst $ eCurrPos h) == firstInFrame) = 
    w { 
      wMessageBuffer = "You can't go there! No turn used.":mBuffer 
      }-- left corner, does not use a turn.
    
  | ((dir == Right) && ((fst $ coord) > (lSize lvl -1))) =  
      nextLevel w -- Right edge of map, does not use a turn, possible issue. Perhaps load next level here.

  | isDoor coord lvl = 
    w {
      wLevel = lvl { 
         lWallTiles = M.delete coord $ lWallTiles lvl
         },
      wHero = h { 
        eOldPos = eCurrPos h, 
        eNextMove = eSpeed h,
        hCurrEnergy = newEnergy
        },
      wMessageBuffer = "Opened door!":mBuffer
      } -- Destroys the door, uses a turn.

  | isMonster coord lvl =
      (simpleCombat h (fromJust $ M.lookup coord (lEntities lvl)) w)
      {
        wHero = h {eOldPos = eCurrPos h,
                   eNextMove = eSpeed h,
                   hCurrEnergy = newEnergy
                  }
      }-- Simple combat (using movement keys)
      
  | (dir == Right) && ((fst $ eCurrPos h) == lastInFrame) && (lastInFrame < lSize lvl) = 
      w { 
        wHero = h { 
           eOldPos = eCurrPos h,
           eCurrPos = coord, 
           eNextMove = eSpeed h,
           hCurrEnergy = newEnergy,
           hMovementSlack = (firstInFrame+1, lastInFrame+1)
           }        
        } -- If at right side of frame 
      
  | otherwise =  
        w { wHero = h { eOldPos = eCurrPos h, eCurrPos = coord, eNextMove = eSpeed h, hCurrEnergy = newEnergy} } -- simple movement, no wrapping. 
  where 
    h              = wHero w
    lvl            = wLevel w
    coord          = (newX, newY)
    newX           = heroX
    newY           = 0
    (heroX, heroY) = eCurrPos h |+| dirToCoord dir
    hConst i       = max 0 (min i 20)
    (firstInFrame, lastInFrame) = hMovementSlack h
    
    mBuffer = wMessageBuffer w
    
    newEnergy = max 0 $ (hCurrEnergy h) - 1

-- simply passes the time.
handleWait :: World -> World
handleWait w = 
  w {
    wHero = h { 
       eOldPos = eCurrPos h,
       eNextMove = eSpeed h
       }, 
    wMessageBuffer = "Waiting!":wMessageBuffer w
    }
  where
    h = wHero w

--Exits!
handleExit :: World -> Assets -> IO ()
handleExit world assets = do
  GUI.shutdown world assets
  
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

-- Show different content in the context window.
handleShow :: Screen -> World -> World
handleShow screen  w = w { wScreenShown = screen }