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


-- First time calculation of hero stats
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
    
    
    

-- Finds which entities whose turns are up, performs AI, those entities whose turns are not up have updated their eNextMove values. Returns a new World with these changes.
think :: World -> IO World
think world = do
  return world''
    where
      h = wHero world
      lvl = wLevel world
      
      e = h:(getEntitiesFromViewFrame world $ getViewFrame world) -- every entity in view       
      
      e' = prepare e -- subtracts the lowest eNextMove value from every entity,
      -- always yielding 1 with 0.
      
      
      h' = fromJust $ find (\x -> case x of  -- find to not have to iterate through the whole thing, fromJust is safe because the hero was added to e.
                               Hero { } -> True                         --  Crashing is a good idea if hero can't be found in e'
                               _ -> False
                           ) e'
      
      e'' =  filter (\x -> (eNextMove x == 0))  e' -- the monsters whose turns are up!
      -- monsters whose turns are now
      monstersAI    = filter monsterFilter e''
      -- monsters whose turns are not up yet.
      monstersWAIT  = (e' \\ [h']) \\ monstersAI
      
      monsterFilter :: Entity -> Bool
      monsterFilter x = case x of
        Monster {} -> True
        _ -> False
        
      -- update the map for monsters whose turns are not yet up!
      emap = foldl (\map m -> updateMap (eCurrPos m) m map) (lEntities lvl) monstersWAIT
      world' = world { wLevel = lvl {lEntities = emap }, wHero = h'}
      
      -- choose an action for the rest of the monsters, execute it, reset time until next move.
      monsters' = map (\m -> m {eNextMove = eSpeed m}) monstersAI
      world'' =  foldl (\w m -> selectAIBehavior m w) world' monsters'

      
       

-- find the lowest timeRemaining, subtract it from all.
prepare :: [Entity] -> [Entity]
prepare e = map (\x -> x { eNextMove = eNextMove x - lowestRemaining }) e
  where
    -- 10000 is infinite in this case.
    lowestRemaining = foldl (\x y -> min x $ eNextMove y) 10000 e
    
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


updateMap :: Position -> Entity -> M.Map Position Entity -> M.Map Position Entity
updateMap oldPos m entityMap = entityMap''
  where
    mPos = eCurrPos m
    
    entityMap' = M.delete oldPos entityMap
    
    entityMap'' = M.insert mPos m entityMap'





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