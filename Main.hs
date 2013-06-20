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
  gameLoop $ world

gameLoop world = do -- entities are the hero, any projectiles and any monsters.
  drawWorld world
  drawEntities world
  world' <- think world
  if hTimeRemaining $ wHero world' == 0 
    then do 
    input <- getInput 
    case input of
      Exit -> handleExit
      Dir dir -> handleDir world' dir
    else do
    gameLoop world'


think world = do
  -- Find all entities currently in view, add them to a list l.
  -- subtract eTimeRemaining from every entity until one or more is 0, save these entities to list l'
  --  for every Inanimate object call move o
  --  for every enemy call ai e
  --  if hero is in l', call getInput

      
    


gameLoop world = do
  drawWorld world
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


-- render the world relative to the player position to enable side scrolling. Only to the right.
handleDir w dir
  | (dir == Left)  && ((fst $ hCurrPos h) == firstInFrame) = 
    gameLoop w { 
      wHero = h { 
         hOldPos = hCurrPos h
         } 
      } -- left corner, no change (takes up a move, possible issue)
                                                     
  | (dir == Right) && ((fst $ hCurrPos h) == lastInFrame) && (lastInFrame < lSize lvl) = 
      gameLoop w { 
        wHero = h { 
           hOldPos = hCurrPos h,
           hCurrPos = coord },
        wLevel = lvl { 
          lViewFrame = (firstInFrame+1, lastInFrame+1) 
          } 
        } -- If at right side of frame
      
  | (fst coord >= lSize lvl) =  
        gameLoop w { 
          wHero = h { 
             hOldPos = hCurrPos h
             } 
          } -- Right edge of map,  uses a turn, possible issue. Perhaps load next level here.
        
  | isDoor coord lvl = gameLoop w { wHero = h { hOldPos = hCurrPos h} }
  | otherwise =  gameLoop w { wHero = h { hOldPos = hCurrPos h, hCurrPos = coord} }
  where 
    h              = wHero w
    lvl            = wLevel w
    coord          = (newX, newY)
    newX           = heroX
    newY           = 0
    (heroX, heroY) = hCurrPos h |+| dirToCoord dir
    hConst i       = max 0 (min i 20)
    
    (firstInFrame, lastInFrame) = lViewFrame lvl
    
    

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