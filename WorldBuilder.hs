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
monstersPer10Bounds = (3, 5) -- n monsters every 10 tiles, so length * 10 div mP10 gives the amount of monsters to be generated.

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
  | nofLevels == 1 = (lvl:prevLevels, g')
  | otherwise = generateLevels g' (nofLevels-1) (lvl:prevLevels)
  where
    (l, g') = randomR lengthBounds g
    (vD, _) = randomR viewDistanceBounds g
    (mP10, _) = randomR monstersPer10Bounds g
    nofMonsters = div l $ div 10 mP10
    
    monsters = generateMonsters g nofMonsters l nofLevels
    floor = generateFloor g l
    wall = generateWall  g l
--    lvl = emptyLevel --strToLevel (walls:[floor])
--    lvl' = lvl {lSize = l, lViewDistance = vD}
    lvl = emptyLevel { lSize = l,
                       lDepth = nofLevels,
                       lViewDistance = vD, 
                       lFloorTiles = floor, 
                       lWallTiles = wall
                     }

generateMonsters :: StdGen -> Int -> Int -> Int -> M.Map Position Entity
generateMonsters g nofMonsters l level = M.empty --monsterMap
  where
    randType = (take nofMonsters $ randoms g) :: [MonsterType]
    randCoords = zip (take nofMonsters $ randomRs (1, (l-1)) g) $ take nofMonsters $ repeat 0
    zippedList = zip randCoords randType
    randMonsters = map (\(c, t) -> baseMonsterEnt { eCurrPos = c,
                                                    eOldPos = c,
                                                    eEntityType = 
                                                      baseMonster { mType = t, 
                                                                    mLevel = level 
                                                                  } 
                                                  }
                       ) zippedList
    monsterMap = M.fromList $ zip randCoords randMonsters

    

generateFloor :: StdGen -> Int -> M.Map Position FloorTile
generateFloor g l = floorMap
  where
    randFloor = (take l $ randoms  g) :: [FloorTile]
--    floor = concat $ map (\i -> if (i == Grass) then "#" else "~") randFloor -- list of all floortiles.
    coords = [(x,y) | x <- [0..], y <- [1]]
    floorMap = M.fromList $ zip coords randFloor

generateWall :: StdGen -> Int -> M.Map Position WallTile
generateWall g l = M.empty -- wallMap
  where
    randWall = (take l $ randoms  g) :: [WallTile]
    coords = [(x,y) | x <- [0..], y <- [0]]
    wallMap = M.fromList $ zip coords randWall