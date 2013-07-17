module WorldBuilder (returnWorld) where

import System.Random
import qualified Data.Map as M
import Data.List


import Level
import Random

import Types.World
import Types.Tiles
import Types.Common
import Types.Monsters

import Content.Base
import Content.Races




lengthBounds :: (Int, Int)
lengthBounds = (100, 200) -- minimum and maximum length of a level
nofLevels = 2 -- TODO: add more.
monsterDistanceBounds :: (Int, Int)
monsterDistanceBounds = (2, 3) -- average distance between monsters
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
generateMonsters :: StdGen -> Int -> Int -> Int -> M.Map Position WallTile -> M.Map Position [Entity]
generateMonsters g nofMonsters l level wallMap = monsterMap
  where
    -- Generates random x coordinates, "rolls again" if one coordinate exists where a door is.
    generateCoords :: StdGen -> Int -> [Position]
    generateCoords _ 0 = []
    generateCoords g_ nofM = (x,0):generateCoords g_' (nofM-1)
      where
        (x, g_') = randX g_
        
        
        randX :: StdGen -> (Int, StdGen)
        randX g__
          | M.member (rX,0) wallMap = randX g__'
          | otherwise = (rX, g__')
            where
              (rX,g__')  = randomR (1, l) g__
        
        
    randCoords = generateCoords g nofMonsters    
    --
    
    
    mIDs = [0..]
    
    generatorList = makeGeneratorList g nofMonsters
    
    zippedList = zip3 mIDs randCoords generatorList
    
    randMonsters = map (\(id, pos,g) -> randomMonster id pos g) zippedList
                   
    filterMonsters :: [Entity] -> (Int,Int) -> [(Position, [Entity])]
    filterMonsters monsters (x,maxX) 
      | x > maxX = []
      | null monstersAtX = filterMonsters monsters (x+1, maxX)
      | otherwise = ((x, 0), monstersAtX):filterMonsters monsters (x+1, maxX) 
      where
        monstersAtX = filter (\m -> (eCurrPos m == (x, 0))) monsters
    
        
    monsterMap = M.fromList $ filterMonsters randMonsters (1, (l-1))




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