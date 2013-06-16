module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

type Position = Int

data World = World { wHero :: Position }

data Input = Up | Down | Left | Right | Exit
           deriving (Eq)

main = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Axis of X"
  gameLoop $ World 0 -- initialized with position 0?

gameLoop world@(World heroX) = do
  drawHero heroX
  input <- getInput
  case input of
    Exit -> handleExit
    _ -> handleDir world input

drawHero heroX = do
  clearScreen
  setCursorPosition 0 heroX
  setSGR [ SetConsoleIntensity BoldIntensity,
           SetColor Foreground Vivid Blue ]
  putStrLn "@"


getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    'a' -> return Left
    's' -> return Down
    'd' -> return Right
    _   -> getInput -- recurse if input not recognized
    
handleDir w@(World heroX) input =
  gameLoop $ w { wHero = boundsCheck newX }
    where
      newX = case input of 
        Up -> heroX
        Left -> heroX - 1
        Down -> heroX
        Right -> heroX + 1
      boundsCheck = (\x -> (max 0 (min x 100)))

handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [ Reset ]
  putStrLn "Thank you for playing Axis of X!"
  
