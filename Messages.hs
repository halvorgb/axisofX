module Messages where

import Types.World

import Helpers


-- mesasge creation.
skillMessage :: SkillResult -> Skill -> Entity -> Entity -> String
skillMessage result skill sourceEnt destEnt =
  case result of
    SUCC -> 
      if sourceEnt == destEnt  -- self skill, no need to pritn target!
      then
        show sourceEnt ++ " successfully used " ++ show skill ++ "!"
      else
        show sourceEnt ++ " successfully used " ++ show skill ++ " on " ++ show destEnt ++ "!"
    MISS -> show sourceEnt ++ " tried to use " ++ show skill ++ " on " ++ show destEnt ++ ",  but missed."
    MIT ->  show sourceEnt ++ " tried to use " ++ show skill ++ " on " ++ show destEnt ++ ",  but to no effect"
    FAT ->  show sourceEnt ++ " tried to use " ++ show skill ++ ", but didn't have enough energy."
    FAIL failureCode -> 
      case failureCode of
        NoTarget -> show sourceEnt ++ " tried to use " ++ show skill ++ ", but failed! (No targets found)"
        CantReach -> show sourceEnt ++ " tried to use " ++ show skill ++ ", but failed! (Couldn't reach dest.)"
    _ -> "Placeholder message \\skillMessage"


mitMessage :: Entity -> Entity -> World -> World
mitMessage sourceEnt destEnt world = 
  world { wMessageBuffer = (show sourceEnt ++ " tried to attack " ++ show destEnt ++ ", but to no effect."):wMessageBuffer world}

missMessage :: Entity -> Entity -> World -> World
missMessage sourceEnt destEnt world =
    world { wMessageBuffer = (show sourceEnt ++ " tried to attack " ++ show destEnt ++ ", but the attack missed."):wMessageBuffer world}

killMessage :: Entity -> Entity -> World -> World
killMessage sourceEnt destEnt world =
    world { wMessageBuffer = (show sourceEnt ++ " killed " ++ show destEnt ++ "."):wMessageBuffer world}

damageMessage :: Entity -> Int -> Entity -> World -> World
damageMessage sourceEnt damage destEnt world = 
    world { wMessageBuffer = (show sourceEnt ++ " dealt " ++ show damage ++ " damage to " ++ show destEnt ++ "."):wMessageBuffer world}

