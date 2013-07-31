module WorldBuilder (returnWorld) where

import System.Random
import qualified Data.Map as M
import Data.List


--import Helpers
import Random

import Types.World
import Types.Tiles
import Types.Common
import Types.Items

import Content.Base
import Content.Races
import Content.MonsterTypes


lengthBounds :: (Int, Int)
lengthBounds = (100, 200) -- minimum and maximum length of a level
nofLevels = 2 -- TODO: add more.
monsterDistanceBounds :: (Int, Int)
monsterDistanceBounds = (5, 8) -- average distance between monsters
doorDistanceBounds :: (Int, Int)
doorDistanceBounds = (20, 40) -- average door distance.





returnWorld :: IO World
returnWorld  = do
  gen <- getStdGen
  let world = generateWorld gen
  return world


generateWorld ::  StdGen -> World
generateWorld gen = world
  where
    (levels, gen') = generateLevels gen nofLevels  []
    world = baseWorld { wLevel = head levels, wLevels = levels, wStdGen = gen' }

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
          
-- Generate numbers that do not occur on the same level and are not placed in tiles with doors.          
-- Possible issue: generators are not split, every temporary list generated uses the same generator.
generateMonsters :: StdGen -> Int -> Int -> Int -> M.Map Position WallTile -> M.Map Position Entity
generateMonsters g nofMonsters l levelDepth wallMap = monsterMap
  where
    doorCoords = getDoorCoords l wallMap
    
    randCoords = randomizeCoordinates nofMonsters l doorCoords g
    
    mIDs = [0..]
    
    (generatorList, _) = makeGeneratorList g nofMonsters
    
    zippedList = zip3 mIDs randCoords generatorList
    
    randMonsters = map (\(id, pos,g) -> randomMonster id pos g) zippedList
    monsterMap = M.fromList $ zip randCoords randMonsters               


-- Helper functions for generateMonsters    
----------------------------------------------                 
getDoorCoords :: Int -> M.Map Position WallTile -> [Position]
getDoorCoords 0 _ = []
getDoorCoords x wallMap
  | doorCheck = (x,0):getDoorCoords (x-1) wallMap
  | otherwise = getDoorCoords (x-1) wallMap
  where
    doorCheck = case M.lookup (x,0) wallMap of
      Just Door -> True
      _ -> False

randomizeCoordinates :: Int -> Int-> [Position] -> StdGen -> [Position]
randomizeCoordinates 0 _ _ _ = []
randomizeCoordinates nofM l takenPositions g_
  = p:randomizeCoordinates (nofM-1)  l (p:takenPositions)  g_'
  where
    (p, g_') = randP g_ l takenPositions
               
-- rerolls if hitting a taken position. Ineffective? probably, but there is plenty of room in the level so i doubt it is an issue.
               
--  improve this if performance is an hindrance.
randP :: StdGen -> Int -> [Position] ->  (Position, StdGen)
randP g__ l takenPositions
  | notElem (rX, 0) takenPositions = ((rX, 0), g__')          
  | otherwise = randP g__' l takenPositions
  where
    (rX, g__') = randomR (1, l) g__
-------------------------------------------
    
-- Generate an actual monster.    
-------------------------------------------
    
-- FUTURE: take into account which level the monster was generated on.
    -- randomize inventories.
    -- new argument: Excluded positions?
randomMonster :: Int -> Position -> StdGen -> Entity
randomMonster mID pos g = createMonster randomMonsterType randomRace inventory level mID pos
  where
    (randomMonsterType, g') = randomListMember monsterTypes g
    
    (randomRace, g'') = randomListMember races g'
    inventory = Inventory [] 0
    -- make level +-2 levels from dungeon level
    level = 1
    

createMonster :: MonsterType -> Race -> Inventory -> Int -> Int -> Position -> Entity
createMonster mt race inv level id position = 
    Monster {  mType = mt,
               mRace = race,
               mInventory = inv,
               mLevel = level,
               mExperienceReward = 1, -- todo
               mSpotted = False,
               mBehaviorStack = mtBehaviorStack mt,
               
               eCurrHP = mHP,
               eMaxHP = mHP,
               mID = id,
               
               
               eCurrPos = position,
               eOldPos = position,
                             
               eSpeed = mSpeed,
               eNextMove = mSpeed,
               
               eHitDie = mHitDie,
               eDamageDie = mDamageDie,
               eEvadeDie = mEvadeDie,
               eMitigation = mMitigation,
               
               eSkillEffects = []
                             
            }
  where
    mHitDie = (mtHitDie mt)
      { dMod = dMod (mtHitDie mt)  +  rHitModifier race }
      
    mEvadeDie = (mtEvadeDie mt)
      { dMod = dMod (mtEvadeDie mt)  + rEvasionModifier race }
      
    mDamageDie = (mtDamageDie mt)
      { dMod = dMod (mtDamageDie mt) + rDamageModifier race }
      
    mMitigation = mtMitigation mt + rMitigationModifier race

    mHP = 
      round $ fromIntegral (mtBaseHP mt + (level * mtHPPerLevel mt)) * rHPMultiplier race
       
    -- todo: experience.
    
    mSpeed = 
      round $ fromIntegral (mtBaseSpeed mt) * rSpeedMultiplier race
    

--------------------------------------
    
    

generateFloor :: StdGen -> Int -> M.Map Position FloorTile
generateFloor g l = floorMap
  where
    randFloor = (take l $ randoms  g) :: [FloorTile]
    coords = [(x,y) | x <- [0..], y <- [1]]
    floorMap = M.fromList $ zip coords randFloor

generateWall :: StdGen -> Int -> Int -> M.Map Position WallTile
generateWall g l nofDoors =  wallMap
  where
    doorCoords = zip (take nofDoors $ randomRs (1, l - 1) g) $ replicate nofDoors 0
    wallMap = M.fromList $ zip doorCoords $ replicate nofDoors Door
    
    
