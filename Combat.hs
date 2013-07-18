module Combat where



import Random
import Types.Common
import Types.World


import qualified Data.Map as M

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



-- Simple combat, just attack rolls and defense rolls.
--- will be used by the hero when moving into an enemy. when the skill queue system is in place.
simpleCombat :: Entity -> Entity -> World -> World
simpleCombat sourceEntity targetEntity world = world'
  where
    oldGen = wStdGen world
    (sourceHitRoll, newGen) = rollDie (eHitDie sourceEntity) oldGen
    (targetEvadeRoll, newGen') = rollDie (eEvadeDie targetEntity) newGen
    
    world' = if sourceHitRoll >= targetEvadeRoll
             then -- hit: roll for damage!
               let (sourceDamageRoll, newGen'') = rollDie (eDamageDie sourceEntity) newGen' in
               let damage = sourceDamageRoll - (eMitigation targetEntity) in
               if damage > 0                 
               then
                 damageEntity sourceEntity targetEntity damage $ world { wStdGen = newGen'' }
               else
                 world { wMessageBuffer = ((show targetEntity) ++ " shrugged off "++(show sourceEntity) ++ "' attack!"):(wMessageBuffer world), wStdGen = newGen'' }
             else
--               world { wMessageBuffer = ("(" ++ (show sourceHitRoll) ++ " < " ++ (show targetEvadeRoll) ++ ")"):
               world { wMessageBuffer = ((show targetEntity) ++ " evaded " ++ (show sourceEntity) ++ "'s attack!"):(wMessageBuffer world), wStdGen = newGen' }


damageEntity :: Entity -> Entity -> Int -> World -> World
damageEntity sourceEntity targetEntity damage world = world'
  where
    newTargetHP = (eCurrHP targetEntity) - damage
    level = wLevel world
    ents = lEntities level
    
    world' = case targetEntity of
      Monster {} -> if newTargetHP > 0
                    then
                      world { wMessageBuffer = ((show targetEntity) ++ " was killed by " ++ (show sourceEntity)):(wMessageBuffer world), 
                              wLevel = level { lEntities = M.delete (eCurrPos targetEntity) ents }
                            }
                    else
                      world { wMessageBuffer = ((show  targetEntity) ++ " took " ++ (show damage) ++ "damage from " ++ (show sourceEntity)):(wMessageBuffer world), 
                              wLevel = level { lEntities = M.insert (eCurrPos targetEntity) (targetEntity {eCurrHP = newTargetHP}) (M.delete (eCurrPos targetEntity) ents) }
                            }
                      
      Hero {} -> world { wMessageBuffer = ((show  targetEntity) ++ " took " ++ (show damage) ++ "damage from " ++ (show sourceEntity)):(wMessageBuffer world), 
                         wHero = targetEntity { eCurrHP = newTargetHP }
                       }
      _ -> world
    