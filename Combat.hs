module Combat where



import Random
import Helpers
import Messages

import Types.Common
import Types.World


import qualified Data.Map as M


-- Simple combat, just attack rolls and defense rolls.
--- will be used by the hero when moving into an enemy. when the skill queue system is in place.
simpleCombat :: Entity -> Entity -> World -> World
simpleCombat sourceEntity targetEntity world =
  if sourceHitRoll >= targetEvadeRoll
  then -- hit: roll for damage!
    let (sourceDamageRoll, newGen'') = rollDie (eDamageDie sourceEntity) newGen'
        damage = sourceDamageRoll - eMitigation targetEntity
    in if damage > 0                 
       then
         damageEntity sourceEntity targetEntity damage $ world { wStdGen = newGen'' }
       else
         mitMessage sourceEntity targetEntity world'
  else
    missMessage sourceEntity targetEntity world'
    
  where
    oldGen = wStdGen world
    (sourceHitRoll, newGen) = rollDie (eHitDie sourceEntity) oldGen
    (targetEvadeRoll, newGen') = rollDie (eEvadeDie targetEntity) newGen
    
    world' = world { wStdGen = newGen' }


damageEntity :: Entity -> Entity -> Int -> World -> World
damageEntity sourceEntity targetEntity damage world = world'
  where
    newTargetHP = eCurrHP targetEntity - damage
    level = wLevel world
    ents = lEntities level
    
    world' = case targetEntity of
      Monster {} -> if newTargetHP < 0
                    then
                      killMessage sourceEntity targetEntity $ 
                      world {wLevel = level { lEntities = M.delete (eCurrPos targetEntity) ents } }

                    else
                      damageMessage sourceEntity damage targetEntity $ 
                      world {wLevel = level { lEntities = M.insert (eCurrPos targetEntity) (targetEntity {eCurrHP = newTargetHP}) (M.delete (eCurrPos targetEntity) ents) } }
                      
      Hero {} -> if newTargetHP < 0
                 then
                   killMessage sourceEntity targetEntity $
                   world {wHero = targetEntity { eCurrHP = newTargetHP }}
                 else
                   damageMessage sourceEntity damage targetEntity $
                   world {wHero = targetEntity { eCurrHP = newTargetHP }}
      
      _ -> error "Unknown target in damageEntity"