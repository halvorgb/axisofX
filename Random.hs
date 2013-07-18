module Random (randomListMember, rollDie, makeGeneratorList, randomMonster) where


import System.Random

import Types.Common
import Types.World
import Types.Items
import Types.World
import Types.MonsterTypes

import Content.MonsterTypes
import Content.Races


-- select random element from list, undefined on empty lists.
randomListMember :: [a] -> StdGen -> (a, StdGen)
randomListMember [] _ = undefined
randomListMember as gen = (a, newGen)
  where
    (index, newGen) = randomR (0, (length as) - 1) gen
    a = as !! index
    
rollDie :: Dice -> StdGen -> (Int, StdGen)
rollDie dice gen = (result, lastGen)
  where
    (nofDie, maxDice) = dDie dice
    modifier = dMod dice
    
    generators = makeGeneratorList gen nofDie
    lastGen = last generators
    
    roll = foldl (+) modifier $ map (\g -> fst $ randomR (1, maxDice) g) generators
    
    result = max roll 0 -- no negative rolls if  negative modifiers + low rolls.
    

makeGeneratorList :: StdGen -> Int -> [StdGen]
makeGeneratorList _ 0 = []
makeGeneratorList gen n = gen1:makeGeneratorList  gen2 (n-1)
  where
    (gen1, gen2) = split gen
    
    
    
    
-- FUTURE: take into account which level the monster was generated on.
    -- randomize inventories.
    -- new argument: Excluded positions?
randomMonster :: Int -> Position -> StdGen -> Entity
randomMonster mID pos g = createMonster randomMonsterType randomRace inventory level mID pos
  where
    (randomMonsterType, g') = randomListMember monsterTypes g
    
    (randomRace, g'') = randomListMember races g'
    inventory = Inventory [] 0
    -- make level +-2 levels from dungeon level
    level = 1
    

createMonster :: MonsterType -> Race -> Inventory -> Int -> Int -> Position -> Entity
createMonster mt race inv level id position = 
    Monster {  mType = mt,
               mRace = race,
               mInventory = inv,
               mLevel = level,
               mExperienceReward = 1, -- todo
               mSpotted = False,
               
               eCurrHP = mHP,
               eMaxHP = mHP,
               mID = id,
               
               
               eCurrPos = position,
               eOldPos = position,
                             
               eSpeed = mSpeed,
               eNextMove = mSpeed,
               
               eHitDie = mHitDie,
               eDamageDie = mDamageDie,
               eEvadeDie = mEvadeDie,
               eMitigation = mMitigation
            }
  where
    mHitDie = (mtHitDie mt)
      { dMod = (dMod $ mtHitDie mt)  +  (rHitModifier race) }
      
    mEvadeDie = (mtEvadeDie mt)
      { dMod = (dMod $ mtEvadeDie mt)  + (rEvasionModifier race) }
      
    mDamageDie = (mtDamageDie mt)
      { dMod = (dMod $ mtDamageDie mt) + (rDamageModifier race) }
      
    mMitigation = (mtMitigation mt) + (rMitigationModifier race)

    mHP = round $ (fromIntegral $ (rBaseHP race) + (level * (rBaseHPPerLevel race))) * (mtHPMultiplier mt)
       
    -- todo: experience.
    
    mSpeed = round $ (fromIntegral $ rBaseSpeed race) * (mtSpeedMultiplier mt)

