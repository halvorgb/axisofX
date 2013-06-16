module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

type Position = Int

data World = World { wHero :: Position }

data Input = Up | Down | Left | Right | Exit
           deriving (Eq)

main = do
  setSGR [ SetConsoleIntensity BoldIntensity,
           SetColor Foreground Vivid Blue ]
  putStrLn "@"
    
