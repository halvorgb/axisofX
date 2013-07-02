module Logic where
import Prelude hiding (Either(..))
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Types



  -- Find all entities currently in view, add them to a list l.
  -- subtract eTimeRemaining from every entity until one or more is 0, save these entities to list l'
  --  for every Inanimate object in l' call move o
  --  for every enemy in l' call ai e
  -- return a new modified world.
  
  -- remove all entities in e from map
  -- do actions
  -- add all entities in e' to map
think world = do 
  return world'
    where
      h = wHero world
      lvl = wLevel world
      e = h:(getEntitiesFromViewFrame world (lViewFrame lvl) (lViewDistance lvl)) -- every entity in view
      e' = prepare e -- subtracts the lowest from every entity
      e'' =  filter (\x -> eTimeUntilNextMove x == 0) e'
      projectiles = filter (\x -> case x of 
                               Entity { eEntityType = Projectile { } } -> True
                               _ -> False
                           ) e''
      monsters    = filter (\x -> case x of 
                               Entity { eEntityType = Monster { } } -> True
                               _ -> False
                           ) e''
                    
      h' = fromJust $ find (\x -> case x of  -- find to not have to iterate through the whole thing, fromJust is safe because the hero was added to e.
                               Entity { eEntityType = Hero { } } -> True                         --  Crashing is a good idea if hero can't be found in e'
                               _ -> False
                           ) e'
           
      projectiles' = move projectiles
      monsters' = ai monsters
      
      -- e''' reset timeUntilNextMove, on every entity except the player.
      e''' = (map (\x -> x { eTimeUntilNextMove = eSpeed x } ) (projectiles' ++ monsters'))
      emap = updateMap (lEntities lvl) (fst $ lViewFrame lvl, (snd $ lViewFrame lvl) + lViewDistance lvl) e'''
      world' = world { wLevel = lvl {lEntities = emap}, wHero = h}
       

getEntitiesFromViewFrame :: World -> (Int, Int) -> Int -> [Entity]
getEntitiesFromViewFrame w (start,_) end
  | start > end = []
  | otherwise   = res ++ getEntitiesFromViewFrame w (start+1,0) end 
  where
    lvl = wLevel w
    res = fromMaybe [] $ M.lookup (start, 0) (lEntities lvl)


-- find the lowest timeRemaining, subtract it from all.
prepare :: [Entity] -> [Entity]
prepare e = sub e
  where
    -- 10000 is infinite in this case.
    lowestRemaining = foldl (\x y -> (min x $ eTimeUntilNextMove y)) 1000 e
    -- subtract this value from every entity
    sub = map (\x -> x {eTimeUntilNextMove = eTimeUntilNextMove x - lowestRemaining})


move :: [Entity] -> [Entity]
move a = a

ai :: [Entity] -> [Entity]
ai a = a

-- Purpose: Given a map, a range and a list of entities, create a  new map and insert every value except those in the range, but insert every value in [Entity]
updateMap :: M.Map Position [Entity] -> (Int, Int) -> [Entity] -> M.Map Position [Entity]
updateMap emap (start, stop) e
  | start > stop = emap
  | otherwise = M.adjust (\_ -> e') pos emap
  where
    pos = (start, 0)
    e' = filter (\x -> eCurrPos x == pos) e




-- just getting a prototype running...
dirToCoord Up    = (0, -1)
dirToCoord Down  = (0,  1)
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)

(|+|) :: Position -> Position -> Position
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
