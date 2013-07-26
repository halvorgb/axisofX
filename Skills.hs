module Skills(performSkills) where

import Data.List

import Types.World

import Helpers
import Random

performSkills :: World -> World
performSkills w =
  foldl' performSkill w indexedSkills
  where
    h = wHero w
    skills = toList $ hSkillQueue h
    index = [1..]
    indexedSkills = zip index skills
    

performSkill :: World -> (Int, Skill) -> World
performSkill w (i,s) =
  if energyCost > hEnergy
  then
    w { wMessageBuffer = skillMessage FAT s h h:wMessageBuffer w}
  else
    if null targets
    then
      w' { wMessageBuffer = skillMessage FAIL s h h:wMessageBuffer w} -- Drain energy on fail? yes? Use Turn? For now YES. (change w' -> w to change to NO)
    else
      
      w''
  where
    l = wLevel w
    h = wHero w
    
    energyCost = skillEnergyCost s h i
    hEnergy = hCurrEnergy h
    speedCost = skillSpeedCost s h 
    
    effects :: [SkillEffect]
    effects = sEffect s    
    -- find all targets, attempt to apply skill effects on said targets. with a fold.
    targets :: [Entity]
    targets = (sTarget s) w
    
--    calculate world with updated nextMove and currEnergy (not used if out of energy.    
    w' = w { wHero = h { hCurrEnergy = hEnergy - energyCost, eNextMove = (eSpeed h) + speedCost } }
    
    
    -- s is sent for constant parameters, foldl because there might be several effects.
    w'' = foldl applyEffect w' $ zip3 effects (repeat targets) (repeat s)
    {-
    afflictedTargets :: [Entity]
    afflictedTargets = map (\t -> 
                             foldl' (\t_ f -> f t_) t fs
                           ) targets

                       -- return world after effects have been applied with a console message, either saying that the action was impossible or successful.
    
    
    lEnts = lEntities l
    
    lEnts' = foldl' (\lE e -> updateMap (eCurrPos e) e lE) lEnts afflictedTargets

-}

applyEffect :: World -> (SkillEffect, [Entity], Skill) -> World
applyEffect world (effect, targets, skill) = 
  case effect of
    FinalConstant { } -> 
      foldl' applyEffectToTarget w' $ zip5 (repeat effect) (repeat skill) (repeat hitRoll) (repeat dmgRoll) targets
      
    FinalScaling  _ scale ->
      let scaledDmg = round $ scale * fromIntegral dmgRoll
      in foldl' applyEffectToTarget w' $ zip5 (repeat effect) (repeat skill) (repeat hitRoll) (repeat scaledDmg) targets
      
    _ -> error "he"
  where
    h = wHero world   
    (hitRoll, g') = rollDie (eHitDie h)  (wStdGen world)
    (dmgRoll, g'') = rollDie (eDamageDie h)  g'
    w' = world { wStdGen = g'' } 


applyEffectToTarget :: World -> (SkillEffect, Skill, Int, Int, Entity) -> World
applyEffectToTarget world (effect, skill, dmg, hit, targetEntity) =
  func skill dmg evdRoll dmg mit targetEntity w'
  
  where
    (evdRoll, g') = rollDie (eEvadeDie targetEntity) (wStdGen world)
    mit = eMitigation targetEntity
    
    w' = world { wStdGen = g' }
      
    
    func = seFunc effect
    