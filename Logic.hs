module Logic where

import Prelude hiding (Either(..))
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Types.World
import Types.Common
import Types.Classes

import Types.Items

import Random
import Combat

import Level

--debug:
import Debug.Trace










-- recalculates hero stats
createHero :: World -> String -> Race -> Class -> World
createHero w n r c  = 
  w { wHero = h {
         hName = n,
         hClass = c,
         hRace = r,
         hInventory = inventory,
         hReputation = reputation,
         hCurrEnergy = energy,
         hMaxEnergy = energy,
         hWield = weapon,
         hWear = armor,
         eCurrHP = hp,
         eMaxHP = hp,
         
         eHitDie = hitDie,
         eDamageDie = dmgDie,
         eEvadeDie = evdDie,
         eMitigation = mitigation
         }
    }
                      
  where
    h = wHero w
    
    inventory = cStartingInventory c
    
    -- todo: experience
    
    reputation = cStartingReputation c
    
    energy = round $ (fromIntegral $ (rBaseEnergy r)) * (cStartingEnergyMultiplier c)
    hp = round $ (fromIntegral $ (rBaseHP r)) * (cStartingHPMultiplier c)
    
    armor = cStartingArmor c
    weapon = cStartingWeapon c
    
    hitDie = (cHitDie c) { dMod = (dMod $ cHitDie c) + (rHitModifier r) + (wepHitBonus weapon) }
    evdDie = (cEvadeDie c) { dMod = (dMod $ cEvadeDie c) + (rEvasionModifier r) + (aEvasion armor)}
    mitigationMod = (cMitigationBonus c) + (rMitigationModifier r)
    damageMod = (cDamageBonus c) + (rDamageModifier r)
    
    dmgDie = (wepDamageDie weapon) { dMod = (dMod $ wepDamageDie weapon) + damageMod}
    mitigation = mitigationMod + (aMitigation armor)
    
    
    









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
  | playerAdjecent = simpleCombat monster hero world --combat monster Prod hero world
  | movementBlocked = world
  | otherwise = moveAI monster playerDirection world
  where
    hero = wHero world
    mPos = eCurrPos monster
    hPos = eCurrPos hero
    playerAdjecent = (mPos |+| (-1, 0) == hPos) || (mPos |+| (1, 0) == hPos)
       
                     
    movementBlocked = isMonster (mPos |+| playerDirection) $ wLevel world
    
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
    
    newEntityMap = updateMap oldPos monster'  oldEntityMap
    
    world' = world { wLevel = level {lEntities = newEntityMap } }

wait :: [Entity] -> M.Map Position Entity -> M.Map Position Entity
wait [] entityMap = entityMap
wait (m:ms) entityMap = wait ms entityMap'
  where
    pos = eCurrPos m
    entityMap' = updateMap pos m entityMap






updateMap :: Position -> Entity -> M.Map Position Entity -> M.Map Position Entity
updateMap oldPos m entityMap = entityMap''
  where
    mPos = eCurrPos m
    
    entityMap' = M.delete oldPos entityMap
    
    entityMap'' = M.insert mPos m entityMap'





-- CheckVision: Purpose: announce newly spotted monsters and bosses in the messageBuffer.
checkVision :: World -> World
checkVision w = w'
  where
    l = wLevel w
    entMap = lEntities l
    
    visibleEs = getEntitiesFromViewFrame w $ getViewFrame w
    
    -- entities that haven't been announced yet.
    newEs = filter (\e -> not $ mSpotted e) visibleEs
    
    markedEs = map (\e -> (eCurrPos e, e {mSpotted = True})) newEs
    
    
    stringBuffer = map (\m -> (show $ snd m) ++ " spotted!") markedEs
    
    entMap' = foldl (\map (key,value) -> updateMap key value map) entMap markedEs
    
    
    w' = w {
      wLevel = l {
         lEntities = entMap'
         },
      wMessageBuffer = stringBuffer ++ (wMessageBuffer w)
      }




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