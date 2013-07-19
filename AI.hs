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
selectBehavior [] w m = undefined -- this should never happen, include wait in behaviorstack to fix.
selectBehavior (f:fs) w m = case f w m of
  Nothing -> selectBehavior fs w m
  Just w' -> w'
  
