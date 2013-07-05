module Render.Console.GUI (setup, update, shutdown) where

import Render.Console.Render
import Render.Console.Layout

import System.Console.ANSI
import System.IO
import GHC.IO.Handle

import Types

setup world = do
  setSGR [ Reset ]
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering -- delay setting of this variable?
  hideCursor
  setTitle "Axis of X"
  clearScreen
  
  -- Clear the area needed for the game (incase terminals do not have black bg)
  
--  hSeek stdout AbsoluteSeek 0
  drawGUI baseLayout

  --makeSpace world
      
update world = do
  drawWorld world viewPortPosition
  drawEntities world viewPortPosition
  
shutdown = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ Reset ]
  putStrLn "thank you for playing Axis of X!"
  

-- Internal functions.

--makeSpace :: (Int, Int) -> Int -> IO ()
--makeSpace (x,y) length = do
drawGUI :: [String] -> IO ()
drawGUI strings = do
  return () 
  {-
  setSGR [SetConsoleIntensity BoldIntensity,
          SetColor Background Dull Black,
          SetColor Foreground Vivid White]
  mapM_ putStrLn strings
  
-}


