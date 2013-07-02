module WorldBuilder where

import Level
import Types

import System.Random
import System.IO.Unsafe
import qualified Data.Map as M

lengthBounds :: (Int, Int)
lengthBounds = (100, 1000) -- minimum and maximum length of a level
nofLevels = 20 -- TODO: add more.
monstersPer10Bounds :: (Int, Int)
monstersPer10Bounds = (3, 4) -- n monsters every 10 tiles, so length * 10 div mP10 gives the amount of monsters to be generated.
doorsPer100Bounds :: (Int, Int)
doorsPer100Bounds = (2, 4)

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
    (mP10, _) = randomR monstersPer10Bounds g
    nofMonsters = div l $ div 10 mP10
    (dP100, _) = randomR doorsPer100Bounds g
    nofDoors = div l $ div 100 dP100
    
    
    (gf, gw) = split g
    
    floor = generateFloor gf l
    wall = generateWall gw l nofDoors


    monsters = generateMonsters g nofMonsters l nofLevels wall

--    lvl = emptyLevel --strToLevel (walls:[floor])
--    lvl' = lvl {lSize = l, lViewDistance = vD}
    lvl = emptyLevel { lSize = l,
                       lDepth = nofLevels,
                       lFloorTiles = floor, 
                       lWallTiles = wall,
                       lEntities = monsters
                     }

generateMonsters :: StdGen -> Int -> Int -> Int -> M.Map Position WallTile -> M.Map Position [Entity]
generateMonsters g nofMonsters l level wallMap = monsterMap
  where
    randType = (take nofMonsters $ randoms g) :: [MonsterType]
    randCoords = zip  -- bit weird, the takeWhile ensures that no monsters spawn on the same tile as a door.
                 (take nofMonsters
                  (takeWhile (\x -> not $ M.member (x,0) wallMap) $ randomRs (1, (l-1)) g)
                 ) 
                 $ replicate nofMonsters 0
                 
                 
    zippedList = zip randCoords randType
    randMonsters = map (\(c, t) -> [baseMonsterEnt { eCurrPos = c,
                                                    eOldPos = c,
                                                    eEntityType = 
                                                      baseMonster { mType = t, 
                                                                    mLevel = level 
                                                                  } 
                                                  }
                                    ]
                       ) zippedList
    monsterMap = M.fromList $ zip randCoords randMonsters

    

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
    randWall = (take l $ randoms  g) :: [WallTile]
    coords = [(x,y) | x <- [0..], y <- [0]]
    wallMap = M.fromList $ zip doorCoords $ replicate nofDoors Door