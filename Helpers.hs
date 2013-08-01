module Helpers where

import Prelude hiding(Either(..))
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Types.World
import Types.Common
import Types.Tiles
import Types.Items



isWater coord lvl = case M.lookup coord (lFloorTiles lvl) of
  Just Water -> True
  _ -> False

isDoor coord lvl = case M.lookup coord (lWallTiles lvl) of
  Just Door -> True
  _ -> False

isGrass coord lvl = case M.lookup coord (lFloorTiles lvl) of
  Just Grass -> True
  _ -> False

isGold coord lvl = M.member coord (lGold lvl)

isMonster coord lvl = case M.lookup coord (lEntities lvl) of 
  Just Monster { } -> True
  _ -> False

isArmor coord lvl = 
  any (\x -> case x of 
          Arm { } -> True
          _ -> False
      ) resList
  where
    resList = fromMaybe [] $ M.lookup coord (lItems lvl)


isPotion coord lvl = 
  any (\x -> case x of 
          Pot { } -> True
          _ -> False
      ) resList
  where
    resList = fromMaybe [] $ M.lookup coord (lItems lvl)

isWeapon coord lvl =  
  any (\x -> case x of 
          Weap { } -> True
          _ -> False
      ) resList
  where
    resList = fromMaybe [] $ M.lookup coord (lItems lvl)

-- Checks if position is clear, used by move functions.
posIsClear :: Position -> World -> Bool
posIsClear coord world =
  not (isMonster coord $ wLevel world)
  &&
  (coord /= heroPos)
  &&
  (fst coord > vfStart) && (fst coord < vfEnd)
  &&
  not (isDoor coord $ wLevel world)
  &&
  (fst coord < levelLength - 1)
  where
    level = wLevel world
    levelLength = lSize level
    heroPos = eCurrPos $ wHero world
    (vfStart, vfEnd) = getViewFrame world


getEntityFromPos :: Position -> World -> Maybe Entity
getEntityFromPos coord w = M.lookup coord (lEntities $ wLevel w)


-- removes entity from oldPos, adds to newPos, updates if the pos is the same.
updateMap :: Position -> Entity -> M.Map Position Entity -> M.Map Position Entity
updateMap oldPos m entityMap = M.insert mPos m $ M.delete oldPos entityMap
  where
    mPos = eCurrPos m


-- CheckVision: Purpose: announce newly spotted monsters and bosses in the messageBuffer.
    -- Given the vision frame, is it necessarry to display the messages?
checkVision :: World -> World
checkVision w =
  w {
    wLevel = l {
       lEntities = entMap'
       },
    wMessageBuffer = stringBuffer ++ wMessageBuffer w
    }
  where
    l = wLevel w
    entMap = lEntities l
    visibleEs = getEntitiesFromViewFrame w $ getViewFrame w
    
    -- entities that haven't been announced yet.
    newEs = filter (not . mSpotted) visibleEs
    markedEs = map (\e -> (eCurrPos e, e {mSpotted = True})) newEs
    stringBuffer = map (\m -> show (snd m) ++ " spotted!") markedEs
    entMap' = foldl' (\map (key,value) -> updateMap key value map) entMap markedEs
    


--- general purpose functions.
------------------------------
-- gets vision of the player, blocked by doors if present etc.
getViewFrame :: World -> (Int, Int) 
getViewFrame world = (vFStart, maxVision)
  where
    lvl = wLevel world
    h = wHero world
    vFStart = fst $ hMovementSlack h
    vFMax = hViewDistance h + snd (hMovementSlack h)
    maxVision = fromMaybe vFMax $ find (\x -> isDoor (x, 0) lvl) [vFStart..vFMax]

-- gets a list of all entities in view.
getEntitiesFromViewFrame :: World -> (Int, Int) -> [Entity]
getEntitiesFromViewFrame w (start,end) = 
  foldl' (\list pos -> maybeToList (M.lookup pos ents) ++ list) [] coordinates
  where
    ents = lEntities $ wLevel w
    coordinates = zip [start..end] $ repeat 0
    

getAndDeleteSkippedEntities :: World -> (Int, Int) -> ([Entity], M.Map Position Entity)
getAndDeleteSkippedEntities w (start, _) =
  foldl' (\(l, e) pos -> ((maybeToList (M.lookup pos e) ++ l), M.delete pos e)) ([], ents) coordinates
  where
    ents = lEntities $ wLevel w
    coordinates = zip [(-200)..(start-1)] $ repeat 0
    
    
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)

(|+|) :: Position -> Position -> Position
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)



entityInRange :: Entity -> (Int, Int) -> Entity -> Bool
entityInRange e1 (minRange, maxRange) e2 = 
  any (\i -> entityAtDistance e1 i e2) [minRange..maxRange]


entityAtDistance :: Entity -> Int -> Entity -> Bool
entityAtDistance e1 distance e2 = 
  (e1Pos |+| (distance, 0) == e2Pos) || (e1Pos |+| (-distance, 0) == e2Pos)
  where
    e1Pos = eCurrPos e1
    e2Pos = eCurrPos e2
    
    
-- Some functions used in Skills and Content.Skills

-- skill, weapon, race, nSkilLQueue.
-- (sEnergyCost + rBaseEnergyCost) * 2*nSkillQ -- TODO: improve this. (bring weapons, classes, possible hero buffs into the picture). (Should have a Hero stat that is recalculated on race/weapon/class/environment changes)
skillEnergyCost :: Skill -> Entity -> Int -> Int
skillEnergyCost s h n =
  case s of
    NoSkill -> 0
    _ -> case h of
      Hero {} -> sEnergyCost s * round (1.25 * fromIntegral n)
      _ -> error "skillEnergyCost on non-hero."
    


-- skill and hero. Should also improve this by including environment changes etc.
skillSpeedCost :: Skill -> Entity -> Int
skillSpeedCost s h =
  case h of
    Hero {} -> round (fromIntegral (eSpeed h) * wepSpeedMultiplier (hWield h) * sSpeedMultiplier s)   
    _ -> error "skillSpeedCost on non-hero"
    
    

-- Moves hero, returns nothing if the tile is occupied.
moveHero :: Position -> World -> Maybe World
moveHero desiredPosition world = 
  if posIsClear desiredPosition world
  then -- pos is free.
    if fst desiredPosition < slackMin -- cant move out of bounds to the left.
    then
      Nothing
    else
      let wrapLength = fst desiredPosition - slackMax
          h' = h { eOldPos = eCurrPos h, eCurrPos = desiredPosition }
          world' = world { wHero = h' }
      in if wrapLength > 0 -- if the level needs to wrap!
         then
           Just world' { wHero = h' { hMovementSlack = (slackMin + wrapLength, slackMax + wrapLength) } }
         else -- nothing special, just move.
           Just world'
  else -- position isn't free.
    Nothing  
  where
    h = wHero world
    (slackMin, slackMax) = hMovementSlack h