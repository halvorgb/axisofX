module LevelGenerator where

import Level
import Types

import System.Random
import System.IO.Unsafe

lengthBounds = (100, 300) -- minimum and maximum length of a level
viewRange = (20, 40) -- minimum and maximum viewRange (+10 for backwards viewing)

-- returnWorld :: I
returnWorld = do
  gen <- getStdGen
  let world = generateWorld gen
  return world


--generateWorld :: StdGen -> IO In
generateWorld gen0 = (num1, num2)
  where
    (num1,gen1) = next gen0
    (num2,gen2) = next gen1
-- generateLevel takes a seed and  generates the whole level.
generateLevel = undefined

--generate :: (Random Seed shit, [(String,String)]) -> (Random Seed shit, [(String,String)])
generate = undefined