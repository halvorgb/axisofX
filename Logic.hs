module Logic where

import Prelude hiding (Either(..))
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Types.World
import Types.Common
import Random


import Level

--debug:
import Debug.Trace


  -- Find all entities currently in view, add them to a list l.
  -- subtract eTimeRemaining from every entity until one or more is 0, save these entities to list l'
  --  for every Inanimate object in l' call move o
  --  for every enemy in l' call ai e
  -- return a new modified world.
  
  -- remove all entities in e from map
  -- do actions
  -- add all entities in e' to map
think :: World -> IO World
think world = do 
  return world''
    where
      h = wHero world
      lvl = wLevel world
      
      viewFrame = getViewFrame world
      
      e = h:(getEntitiesFromViewFrame world viewFrame) -- every entity in view
      
      dbg ents
        | trace ("\n\ndbg" ++ "\n" ++ show ents) True = True
        
      
      e' = prepare e -- subtracts the lowest from every entity
      
      
      h' = fromJust $ find (\x -> case x of  -- find to not have to iterate through the whole thing, fromJust is safe because the hero was added to e.
                               Hero { } -> True                         --  Crashing is a good idea if hero can't be found in e'
                               _ -> False
                           ) e'
      
      e'' =  filter (\x -> (eNextMove x == 0))  e' -- the monsters whose turns are up!
      monstersAI    = filter monsterFilter e''
      monstersWAIT  = (e' \\ [h']) \\ monstersAI
      
      
      monsterFilter x = case x of
        Monster {} -> True
        _ -> False
        
      -- UPDATE ZONE (update the map for monsters whose turns are not yet up!)  
      emap = wait monstersWAIT $ lEntities lvl              
      
      world' = world { wLevel = lvl {lEntities = emap }, wHero = h'}
      -- /UPDATE ZONE
      
      -- AI ZONE
      monsters' = map (\m -> m {eNextMove = eSpeed m}) monstersAI
      world'' =  ai monsters' world'
      -- /AI ZONE
      
       

-- find the lowest timeRemaining, subtract it from all.
prepare :: [Entity] -> [Entity]
prepare e = map (\x -> x { eNextMove = eNextMove x - lowestRemaining }) e
  where
    -- 10000 is infinite in this case.
    lowestRemaining = foldl (\x y -> min x $ eNextMove y) 10000 e


-- recur through every entity whose turn is up, do an action and update the world
ai :: [Entity] -> World -> World
ai [] world = world
ai (m:ms) world = ai ms world'
  where
    world' = selectAIBehavior m world
    
selectAIBehavior :: Entity -> World -> World
selectAIBehavior monster world
  | playerAdjecent = world --combat monster Prod hero world
  | otherwise = moveAI monster playerDirection world
  where
    hero = wHero world
    mPos = eCurrPos monster
    hPos = eCurrPos hero
    playerAdjecent = (mPos |+| (-1, 0) == hPos) || (mPos |+| (1, 0) == hPos)
    
    playerDirection = if (fst mPos) > (fst hPos)
                      then
                        dirToCoord Left
                      else
                        dirToCoord Right
    
    
moveAI :: Entity -> (Int, Int) -> World -> World
moveAI monster dir world = world'
  where
    oldPos = eCurrPos monster
    level = wLevel world
    oldEntityMap = lEntities level
    
    monster' = monster { eOldPos = eCurrPos monster,
                         eCurrPos = eCurrPos monster |+| dir
                       }
    
    newEntityMap = updateMap monster' oldPos oldEntityMap
    
    world' = world { wLevel = level {lEntities = newEntityMap } }

wait :: [Entity] -> M.Map Position Entity -> M.Map Position Entity
wait [] entityMap = entityMap
wait (m:ms) entityMap = wait ms entityMap'
  where
    pos = eCurrPos m
    entityMap' = updateMap m pos entityMap






updateMap :: Entity -> Position -> M.Map Position Entity -> M.Map Position Entity
updateMap m pos entityMap = entityMap'
  where
    
    mPos = eCurrPos m
    
    entityMap' =
      if mPos == pos -- Position has not changed, simply update the value.
      then
        M.insert pos m entityMap
      else
        M.insert pos m $ M.delete mPos entityMap'
        {-
    
    -- remove the mID (monster) from pos (if it exists)    
    esAtPos = fromMaybe [] $ M.lookup pos entityMap
    
    esAtPosF = filter (\m' -> case m' of
                          Monster {} -> mID m' /= mID m
                          _ -> False
                      ) esAtPos 
    -- remove m from pos
    entityMap' =
      if null esAtPos -- no previous entry at pos
      then
        entityMap -- no change
      else
        if null esAtPosF -- "Only "e" is at key pos.
        then
          M.delete pos entityMap -- delete said key
          
        else  -- There are others in addition to "e" at key pos
          M.insert pos esAtPosF -- add the others 
          (M.delete pos entityMap) -- remove "e" 
    
    
    -- add m to mPos (can equal pos)
    entityMap'' = M.insertWith (\new olde -> new ++ olde) (mPos) [m] entityMap'

-}










-- Combat!
--calculateDamage :: Entity -> AttackType -> Entity -> Int
--calculateDamage sourceEnt ackType destEnt = 1


--                   v<-- AttackType
combat :: Entity -> Int -> [Entity] -> World -> World
combat sourceEnt atckType destEnts world
  | null destEnts = 
    let failureString = "You " ++ (show atckType) ++ " at nothing, and miss!" in
          world { wMessageBuffer = failureString:(wMessageBuffer world) }
  | otherwise = world



-- Simple combat, just attack rolls and defense rolls against a random target,
--- will be used by the hero when moving into an enemy. when the skill queue system is in place.
simpleCombat :: Entity -> Entity -> World -> World
simpleCombat sourceEntity targetEntity world = world'
  where
    oldGen = wStdGen world
    

    
    (sourceHitRoll, newGen) = rollDie (eHitDie sourceEntity) oldGen
    (targetEvadeRoll, newGen') = rollDie (eEvadeDie targetEntity) newGen'
    
    world' = if sourceHitRoll >= targetEvadeRoll
             then -- hit: roll for damage!
               world { wMessageBuffer = "Temp hit message!":(wMessageBuffer world), wStdGen = newGen' }
             else
               world { wMessageBuffer = ("(" ++ (show sourceHitRoll) ++ " < " ++ (show targetEvadeRoll) ++ ")"):((show sourceEntity) ++ " tried to hit " ++ (show targetEntity) ++ ", but missed!"):(wMessageBuffer world), wStdGen = newGen' }







--- general purpose functions.

getViewFrame :: World -> (Int, Int) -- gets vision of the player, blocked by doors if present etc.
getViewFrame world = (vFStart, maxVision)
  where
    lvl = wLevel world
    h = wHero world
    vFStart = fst $ hMovementSlack h
    vFMax = hViewDistance h + (snd $ hMovementSlack h)
    maxVision = fromMaybe vFMax $ find (\x -> isDoor (x, 0) lvl) [vFStart..vFMax]

getEntitiesFromViewFrame :: World -> (Int, Int) -> [Entity]
getEntitiesFromViewFrame w (start,end)
  | start > end    = []
  | res == Nothing = getEntitiesFromViewFrame w (start+1, end)
  | otherwise      = (fromJust res):getEntitiesFromViewFrame w (start+1,end)
  where
    lvl = wLevel w
    res =  M.lookup (start, 0) (lEntities lvl)
    
    
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)

(|+|) :: Position -> Position -> Position
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)