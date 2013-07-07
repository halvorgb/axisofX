module Logic where
import Prelude hiding (Either(..))
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Types
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
  return world'
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
      -- /UPDATE ZONE

      -- AI ZONE
      monsters' = map (\m -> m {eNextMove = eSpeed m}) monstersAI
      emap' =  ai monsters' emap
      -- /AI ZONE
      
      
      
      world' = world { wLevel = lvl {lEntities = emap'}, wHero = h'}
      

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
  | start > end = []
  | otherwise   = res ++ getEntitiesFromViewFrame w (start+1,end)
  where
    lvl = wLevel w
    res = fromMaybe [] $ M.lookup (start, 0) (lEntities lvl)


-- find the lowest timeRemaining, subtract it from all.
prepare :: [Entity] -> [Entity]
prepare e = map (\x -> x { eNextMove = eNextMove x - lowestRemaining }) e
  where
    -- 10000 is infinite in this case.
    lowestRemaining = foldl (\x y -> min x $ eNextMove y) 10000 e


-- recurse through every entity whose turn is up, do an action and update the map
    -- currently only moves to the left.
ai :: [Entity] -> M.Map Position [Entity] -> M.Map Position [Entity]
ai [] entityMap = entityMap
ai (m:ms) entityMap = ai ms entityMap'
  where
    oldPos = eCurrPos m
    m' = moveLeft m

    moveLeft :: Entity -> Entity
    moveLeft e =
      e { eOldPos = eCurrPos e,
          eCurrPos = ((fst $ eCurrPos e) - 1, snd $ eCurrPos e)
        }
    entityMap' = updateMap m' oldPos entityMap
    
wait :: [Entity] -> M.Map Position [Entity] -> M.Map Position [Entity]
wait [] entityMap = entityMap
wait (m:ms) entityMap = wait ms entityMap'
  where
    pos = eCurrPos m
    entityMap' = updateMap m pos entityMap


-- just getting a prototype running...
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)

(|+|) :: Position -> Position -> Position
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)





updateMap :: Entity -> Position -> M.Map Position [Entity] -> M.Map Position [Entity]
updateMap m pos entityMap = entityMap''
  where
    
    mPos = eCurrPos m
    
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






-- Combat!
damageEntity :: Entity -> (Int, Int) -> Entity
damageEntity e (minDamage, maxDamage) = e
