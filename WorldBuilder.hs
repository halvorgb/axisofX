module WorldBuilder (returnWorld) where

import System.Random
import qualified Data.Map as M
import Data.List


import Level
import Random

import Types.World
import Types.Tiles
import Types.Common
import Types.MonsterTypes

import Content.Base
import Content.Races





lengthBounds :: (Int, Int)
lengthBounds = (100, 200) -- minimum and maximum length of a level
nofLevels = 2 -- TODO: add more.
monsterDistanceBounds :: (Int, Int)
monsterDistanceBounds = (5, 8) -- average distance between monsters
doorDistanceBounds :: (Int, Int)
doorDistanceBounds = (20, 40) -- average door distance.





returnWorld :: IO (World)
returnWorld  = do
  gen <- getStdGen
  let world = generateWorld gen
  return world


generateWorld ::  StdGen -> World
generateWorld gen = world
  where
    (levels, gen') = generateLevels gen nofLevels  []
    world = baseWorld { wLevel = (levels !! 0), wLevels = levels, wStdGen = gen' }

generateLevels :: StdGen -> Int -> [Level] -> ([Level], StdGen)
generateLevels g nofLevels prevLevels
  | nofLevels == 1 = (lvl:prevLevels, g')
  | otherwise = generateLevels g' (nofLevels-1) (lvl:prevLevels)
  where
    (l, g') = randomR lengthBounds g
    (mD, _) = randomR monsterDistanceBounds g
    nofMonsters = div l mD
    (dD, _) = randomR doorDistanceBounds g
    nofDoors = div l dD
    
    (gf, gw) = split g
    
    floor = generateFloor gf l
    wall = generateWall gw l nofDoors

    monsters = generateMonsters g nofMonsters l nofLevels wall

    lvl = baseLevel { lSize = l,
                      lDepth = nofLevels,
                      lFloorTiles = floor, 
                      lWallTiles = wall,
                      lEntities = monsters
                    }
-- Possible issue: generators are not split, every temporary list generated uses the same generator.
generateMonsters :: StdGen -> Int -> Int -> Int -> M.Map Position WallTile -> M.Map Position Entity
generateMonsters g nofMonsters l levelDepth wallMap = monsterMap
  where
    
    getDoorCoords :: Int -> [Position]
    getDoorCoords 0 = []
    getDoorCoords x
      | doorCheck = (x,0):getDoorCoords (x-1)
      | otherwise = getDoorCoords (x-1)
      where
        doorCheck = case M.lookup (x,0) wallMap of
          Just Door -> True
          _ -> False
    doorCoords = getDoorCoords l
    
    
    randomizeCoordinates :: Int -> [Position] -> StdGen -> [Position]
    randomizeCoordinates 0 _ _ = []
    randomizeCoordinates nofM takenPositions g_
      = p:randomizeCoordinates (nofM-1) (p:takenPositions) g_'
      where
        (p, g_') = randP g_ 
        
        -- rerolls if hitting a taken position. Ineffective? probably, but there is plenty of room in the level so i doubt it is an issue.
        
        --  improve this if performance is an hindrance.
        randP :: StdGen ->  (Position, StdGen)
        randP g__ 
          | notElem (rX, 0) takenPositions = ((rX, 0), g__')          
          | otherwise = randP g__'
          where
            (rX, g__') = randomR (1, l) g__
    
    
    randCoords = randomizeCoordinates l doorCoords g
    
    mIDs = [0..]
    
    generatorList = makeGeneratorList g nofMonsters
    
    zippedList = zip3 mIDs randCoords generatorList
    
    randMonsters = map (\(id, pos,g) -> randomMonster id pos g) zippedList
    monsterMap = M.fromList $ zip randCoords randMonsters               




    --monsterMap = M.fromList $ zip randCoords randMonsters

    

generateFloor :: StdGen -> Int -> M.Map Position FloorTile
generateFloor g l = floorMap
  where
    randFloor = (take l $ randoms  g) :: [FloorTile]
--    floor = concat $ map (\i -> if (i == Grass) then "#" else "~") randFloor -- list of all floortiles.
    coords = [(x,y) | x <- [0..], y <- [1]]
    floorMap = M.fromList $ zip coords randFloor

generateWall :: StdGen -> Int -> Int -> M.Map Position WallTile
generateWall g l nofDoors =  wallMap
  where
    doorCoords = zip (take nofDoors $ randomRs (1, (l-1)) g) $ replicate nofDoors 0
    wallMap = M.fromList $ zip doorCoords $ replicate nofDoors Door