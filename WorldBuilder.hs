module WorldBuilder where

import Level
import Types

import System.Random
import System.IO.Unsafe
import qualified Data.Map as M

lengthBounds :: (Int, Int)
lengthBounds = (100, 200) -- minimum and maximum length of a level
viewDistanceBounds :: (Int, Int)
viewDistanceBounds = (5, 40) -- minimum and maximum viewRange (+10 for backwards viewing)
nofLevels = 1 -- TODO: add more.
monstersPer10Bounds :: (Int, Int)
monstersPer10Bounds = (3, 7) -- n monsters every 10 tiles, so length div mP10 gives the amount of monsters to be generated.

-- returnWorld :: I
returnWorld = do
  gen <- getStdGen
  let world = generateWorld gen
  return world


--generateWorld :: StdGen -> (Int, Int)
generateWorld gen = world
  where
    (levels, _) = generateLevels gen nofLevels  []
    world = genesis { wLevel = (levels !! 0), wLevels = levels }

generateLevels :: StdGen -> Int -> [Level] -> ([Level], StdGen)
generateLevels g nofLevels prevLevels
  | nofLevels == 1 = (lvl':prevLevels, g')
  | otherwise = generateLevels g' (nofLevels-1) (lvl:prevLevels)
  where
    (l, g') = randomR lengthBounds g
    (vD, g'') = randomR viewDistanceBounds g'
    (mP10, g''') = randomR monstersPer10Bounds g''
    nofMonsters = div l mP10
    (monsters, g'''') = generateMonsters g''' nofMonsters l
    floor = generateFloor g'''' l
    walls = concat $ generateWalls l
    lvl = strToLevel (walls:[floor])
    lvl' = lvl {lSize = l, lViewDistance = vD}
    

generateMonsters :: StdGen -> Int -> Int -> (String, StdGen)
generateMonsters g nofMonsters l = ([], g)


generateFloor :: StdGen -> Int -> String
generateFloor g l = floor
  where
    possibleValues = "#~"
    randInts = (take l $ randomRs (1,2) g) :: [Int]
    floor = concat $ map (\i -> if (i == 1) then "#" else "~") randInts

generateFloor' :: StdGen -> Int -> M.Map Position Entity
generateFloor' g l = undefined
  where
    lst = M.empty


strToLevel :: [String] -> Level
strToLevel str = foldl populate emptyLevel {lSize = maxX} asciiMap
  where
    asciiMap = concat $ zipWith zip coords str
    coords = [[(x,y) | x <- [0..]] | y <- [0..]]
    maxX = maximum . map (fst . fst ) $ asciiMap
    populate lvl (coord, tile) =
      case tile of
        '#' -> lvl { lTiles = M.insert coord Floor t }
        '~' -> lvl { lTiles = M.insert coord Water t }
        '|' -> lvl { lTiles = M.insert coord Door t }
--        '\\' -> lvl { lTiles = M.insert coord Stairs t }
        _ -> lvl
        where
          t = lTiles lvl
                     


generateWalls 1 = []
generateWalls l = " ":generateWalls (l-1)