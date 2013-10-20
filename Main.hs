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
import Player

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
gameLoop world assets
  | null (wLevels world) || 
    (eCurrHP (wHero world) < 0) || 
    fst (eCurrPos (wBoss world)) > fst (eCurrPos (wHero world)) = -- game end
      GUI.delayedShutdown world assets
  | eNextMove (wHero world) == 0 = do -- player's turn
    let world' = checkVision world
    GUI.update_ world' assets
    input <- GUI.getInput
    case input of 
      Show screen -> gameLoop (handleShow screen world') assets
      Exit -> handleExit (world' {wScreenShown = Console}) assets
      Wait -> gameLoop (handleWait (world' {wScreenShown = Console})) assets
      Rest -> gameLoop (handleRest (world' {wScreenShown = Console})) assets
      Dir dir -> gameLoop (handleDir (world' {wScreenShown = Console}) dir) assets
      Queue i -> queueSkill i (world' {wScreenShown = Skills}) assets
      ExecuteSkills -> executeSkills (world' {wScreenShown = Console}) assets
  | otherwise = do -- ai's turn
      world' <- think world
      gameLoop world' assets


-- tries to move the player.
      -- if a door is encountered, open it.
      -- if an enemy is encountered: attack it,
handleDir :: World -> Direction -> World
handleDir w dir
  | dir == Left && fst (eCurrPos h) == firstInFrame = -- left corner, does not use a turn.
    w { 
      wMessageBuffer = "You can't go there! No turn used.":mBuffer 
      }
    
  | dir == Right && fst coord > lSize lvl - 1 =  -- Right edge of map, does not use a turn, possible issue. Perhaps load next level here.
      nextLevel $ w { wTimeElapsed = newTime }  

  | isDoor coord lvl = -- Destroys the door, uses a turn.
    w {
      wLevel = lvl { 
         lWallTiles = M.delete coord $ lWallTiles lvl
         },
      wHero = h { 
        eOldPos = eCurrPos h, 
        eNextMove = eSpeed h,
        hCurrEnergy = newEnergy
        },
      wTimeElapsed = newTime,
      wMessageBuffer = "Opened door!":mBuffer
      }

  | isMonster coord lvl = -- Simple combat (using movement keys)
      (simpleCombat h (fromJust $ M.lookup coord (lEntities lvl)) w)
      {
        wHero = h {eOldPos = eCurrPos h,
                   eNextMove = eSpeed h,
                   hCurrEnergy = newEnergy
                  },
        wTimeElapsed = newTime
      }
      
  | dir == Right && fst (eCurrPos h) == lastInFrame && lastInFrame < lSize lvl = -- If at right side of frame 
      w { 
        wHero = h { 
           eOldPos = eCurrPos h,
           eCurrPos = coord, 
           eNextMove = eSpeed h,
           hCurrEnergy = newEnergy,
           hMovementSlack = (firstInFrame+1, lastInFrame+1)
           },
        wTimeElapsed = newTime   
        }
      
  | otherwise = -- simple movement, no wrapping. 
        w { wHero = h { eOldPos = eCurrPos h, eCurrPos = coord, eNextMove = eSpeed h, hCurrEnergy = newEnergy}, 
            wTimeElapsed = newTime
          } 
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
    
    newEnergy = max 0 $ hCurrEnergy h - 1
    newTime = wTimeElapsed w + fromIntegral (eSpeed h)

-- simply passes the time.
handleWait :: World -> World
handleWait w = 
  w {
    wHero = h { 
       eOldPos = eCurrPos h,
       eNextMove = eSpeed h
       }, 
    wTimeElapsed = fromIntegral (eSpeed h) + wTimeElapsed w,
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
      world { wLevel =  wLevels world !! 1, 
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


-- Rests the player, fails if enemies are nearby.
handleRest :: World -> World
handleRest w = 
  if null enemiesInView -- Safe to rest?
  then
    if hMaxEnergy h == hCurrEnergy h
    then
      w { wMessageBuffer = "You're already at maximum energy!":wMessageBuffer w }
    else
      w { wHero = h { hCurrEnergy = hMaxEnergy h, eNextMove = fromIntegral newTime}, wMessageBuffer = "You wake up from your rest brimming with energy.":wMessageBuffer w, wTimeElapsed = newTime}
  else
    w { wMessageBuffer = "You can't rest while enemies are nearby!":wMessageBuffer w}
  where
    h = wHero w
    enemiesInView = getEntitiesFromViewFrame w $ getViewFrame w
    
    newTime = 500 + wTimeElapsed w + fromIntegral((hMaxEnergy h - hCurrEnergy h) * 2)
  


-- handles skill queue (duh), asks for input.
queueSkill :: Int -> World -> Assets -> IO ()
queueSkill n w a = do
  skill <- chooseSkill w a
  
  let w' = case skill of
        NoSkill -> w { wHero = h { hSkillQueue = removeFromQueue n $ hSkillQueue h},                 
                       wScreenShown = Console
                     }
        _ ->  w { wHero = h { hSkillQueue = addToQueue skill n $ hSkillQueue h}, 
                  wScreenShown = Console
                }
  gameLoop w' a
  where
    h = wHero w
    

-- executes queued skills. (need  to check for energy etc)
executeSkills :: World -> Assets -> IO ()
executeSkills w a = gameLoop (performSkills w) a  
    
   
  
  