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
    
posIsClear :: Position -> World -> Bool
posIsClear coord world =
  (not $ isMonster coord $ wLevel world)
  &&
  (coord /= heroPos)
  where
    heroPos = eCurrPos $ wHero world



-- removes entity from oldPos, adds to newPos, updates if the pos is the same.
updateMap :: Position -> Entity -> M.Map Position Entity -> M.Map Position Entity
updateMap oldPos m entityMap = M.insert mPos m $ M.delete oldPos entityMap
  where
    mPos = eCurrPos m


-- CheckVision: Purpose: announce newly spotted monsters and bosses in the messageBuffer.
checkVision :: World -> World
checkVision w =
  w {
    wLevel = l {
       lEntities = entMap'
       },
    wMessageBuffer = stringBuffer ++ (wMessageBuffer w)
    }
  where
    l = wLevel w
    entMap = lEntities l
    
    visibleEs = getEntitiesFromViewFrame w $ getViewFrame w
    
    -- entities that haven't been announced yet.
    newEs = filter (\e -> not $ mSpotted e) visibleEs
    
    markedEs = map (\e -> (eCurrPos e, e {mSpotted = True})) newEs
    
    stringBuffer = map (\m -> (show $ snd m) ++ " spotted!") markedEs
    
    entMap' = foldl (\map (key,value) -> updateMap key value map) entMap markedEs
    


--- general purpose functions.
------------------------------
-- gets vision of the player, blocked by doors if present etc.
getViewFrame :: World -> (Int, Int) 
getViewFrame world = (vFStart, maxVision)
  where
    lvl = wLevel world
    h = wHero world
    vFStart = fst $ hMovementSlack h
    vFMax = hViewDistance h + (snd $ hMovementSlack h)
    maxVision = fromMaybe vFMax $ find (\x -> isDoor (x, 0) lvl) [vFStart..vFMax]

-- gets a list of all entities in view.
getEntitiesFromViewFrame :: World -> (Int, Int) -> [Entity]
getEntitiesFromViewFrame w (start,end) = foldl (\list pos -> (fromMaybe [] $ fmap (:[]) $ M.lookup pos ents) ++ list) [] coordinates
  where
    ents = lEntities $ wLevel w
    coordinates = zip [start..end] $ repeat 0
    
    
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)

(|+|) :: Position -> Position -> Position
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)



entityInRange :: Entity -> (Int, Int) -> Entity -> Bool
entityInRange e1 (minRange, maxRange) e2 = 
  any (\i -> entityAtDistance e1 i e2) [minRange..maxRange]


entityAtDistance :: Entity -> Int -> Entity -> Bool
entityAtDistance e1 distance e2 = 
  (e1Pos |+| (distance, 0) == e2Pos) || (e1Pos |+| ((-distance), 0) == e2Pos)
  where
    e1Pos = eCurrPos e1
    e2Pos = eCurrPos e2
    