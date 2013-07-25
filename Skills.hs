module Skills(performSkills) where

import Data.List

import Types.World

import Helpers

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
  w { wLevel = l { lEntities = lEnts' }}    
  where
    l = wLevel w
    fs :: [SkillEffect]
    fs = sEffect s    
    -- find all targets, attempt to apply skill effects on said targets. with a fold.
    targets :: [Entity]
    targets = (sTarget s) w
    
    afflictedTargets :: [Entity]
    afflictedTargets = map (\t -> 
                             foldl' (\t_ f -> f t_) t fs
                           ) targets    
                       -- return world after effects have been applied with a console message, either saying that the action was impossible or successful.
    
    
    lEnts = lEntities l
    
    lEnts' = foldl' (\lE e -> updateMap (eCurrPos e) e lE) lEnts afflictedTargets



affectEntity :: Entity -> [SkillEffect] -> World -> World
affectEntity e effect w = w
  where
    lol = undefined




performSkillEffect :: Entity -> Entity -> SkillEffect -> World -> World
preformSkillEffect sourceEnt destEnt effect world = world
  where
    stat = seAffectedStat effect
    newStatValue = case stat of
      Stat_HP  -> undefined
      Stat_NRG -> undefined
      Stat_Hit -> undefined
      Stat_Dmg -> undefined
      Stat_Mit -> undefined
      Stat_Spd -> undefined
      Stat_Pos -> undefiend
      


      
    