module AI where

import Prelude hiding(Either(..))
import Data.List
import Data.Maybe
import Types.World
import Types.Common

                        
performAI :: Entity -> World -> World
performAI monster world = selectBehavior behaviorStack world monster
  where
    behaviorStack = mBehaviorStack monster
    
selectBehavior :: [World -> Entity -> Maybe World] -> World -> Entity -> World
selectBehavior [] w m = error "No behaviors found in a monster's behaviorstack. To fix, ensure wait is at the end of every behaviorstack."
selectBehavior (f:fs) w m = 
  fromMaybe (selectBehavior fs w m) (f w m)
  
  

bossAI :: [Entity] -> World -> World
bossAI skippedEntities world
  | eNextMove b == 0 =
    world { wBoss = b {eNextMove = eSpeed b, eCurrPos = (1 + fst (eCurrPos b), snd $ eCurrPos b), eOldPos = eCurrPos b, bRivalKills = bRivalKills b + length skippedEntities } }
  | otherwise = 
    world { wBoss = b { bRivalKills = bRivalKills b + length skippedEntities } }
  where
    b = wBoss world
    
