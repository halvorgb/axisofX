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
  if energyCost > hEnergy
  then
    w { wMessageBuffer = skillMessage FAT s h h:wMessageBuffer w}
  else
    w
--  w { wLevel = l { lEntities = lEnts' }}    
  where
    l = wLevel w
    h = wHero w
    
    energyCost = skillEnergyCost s h i
    hEnergy = hCurrEnergy h
    
    fs :: [SkillEffect]
    fs = sEffect s    
    -- find all targets, attempt to apply skill effects on said targets. with a fold.
    targets :: [Entity]
    targets = (sTarget s) w
    {-
    afflictedTargets :: [Entity]
    afflictedTargets = map (\t -> 
                             foldl' (\t_ f -> f t_) t fs
                           ) targets

                       -- return world after effects have been applied with a console message, either saying that the action was impossible or successful.
    
    
    lEnts = lEntities l
    
    lEnts' = foldl' (\lE e -> updateMap (eCurrPos e) e lE) lEnts afflictedTargets

-}

affectEntity :: Entity -> [SkillEffect] -> World -> World
affectEntity e effect w = w
  where
    lol = undefined

    