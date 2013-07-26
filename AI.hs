module AI where

import Prelude hiding(Either(..))
import Data.List

import Types.World
import Types.Common

    
                        
performAI :: Entity -> World -> World
performAI monster world = selectBehavior behaviorStack world monster
  where
    behaviorStack = mBehaviorStack monster
    
selectBehavior :: [World -> Entity -> Maybe World] -> World -> Entity -> World
selectBehavior [] w m = error "No behaviors found in a monster's behaviorstack. To fix, ensure wait is at the end of every behaviorstack."
selectBehavior (f:fs) w m = case f w m of
  Nothing -> selectBehavior fs w m
  Just w' -> w'
  
