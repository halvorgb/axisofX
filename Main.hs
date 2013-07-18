module Main where

import Prelude hiding (Either(..))
import System.IO
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Render.SDL.GUI as GUI
import Render.SDL.Render
import Combat
import Level
import Logic
import WorldBuilder

import Types.World
import Types.Common
import Types.Classes

import Content.Classes
import Content.Races



main :: IO ()
main = do
  assets <- loadAssets
  GUI.setup
  
  (name, race, klass) <- GUI.chooseProtagonist assets
  
  world <- returnWorld
  

  let world' = createHero world name klass race

  
  gameLoop world' assets


gameLoop :: World -> Assets -> IO ()
gameLoop world assets = do
  if (null $ wLevels world) || ((eCurrHP $ wHero world) <= 0) -- check if complete. (future also check for death)
    then do
    GUI.shutdown world assets
    else do
    if (eNextMove $ wHero world) == 0  -- check if hero's turn.
      then do 
      let world' = checkVision world
      GUI.update_ world' assets
      input <- GUI.getInput
      case input of 
        Show screen -> gameLoop (handleShow screen world') assets
        Exit -> handleExit world' assets
        Wait -> gameLoop (handleWait world') assets
        Dir dir -> gameLoop (handleDir world' dir) assets
      else do -- else: AI
      world' <- think world
      gameLoop world' assets



handleDir :: World -> Direction -> World
handleDir w dir
  | (dir == Left)  && ((fst $ eCurrPos h) == firstInFrame) = 
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
        } -- If at right side of frame -- 
      
  | otherwise =  
        w { wHero = h { eOldPos = eCurrPos h, eCurrPos = coord, eNextMove = eSpeed h, hCurrEnergy = newEnergy} }
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




handleShow :: Screen -> World -> World
handleShow screen  w = w { wScreenShown = screen }