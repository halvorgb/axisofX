module Content.MonsterTypes where

import Types.Common
import Types.World

import Helpers
import Combat

monsterTypes = [fighter, crossbowman]


-- common behaviors: 
----------------------------------------
-- this behavior should always be at the bottom of the stack.
bhvWait :: World -> Entity -> Maybe World
bhvWait world monster = Just world -- nextMove is handled in Logic.

bhvMeleeCombat :: World -> Entity -> Maybe World
bhvMeleeCombat world monster
  | not playerAdjecent = Nothing -- player isn't close enough, abort dat shit.
  | otherwise = Just $ simpleCombat monster hero world -- actual function here.
  where
    hero = wHero world
    playerAdjecent = entityAtDistance monster 1 hero
                     
-- moves 1 tile towars the player, uses bhvMoveNTiles.
bhvMove1TowardsPlayer :: World -> Entity -> Maybe World
bhvMove1TowardsPlayer world monster = bhvMoveNTiles world monster distance
  where
    hero = wHero world
    oldPos = eCurrPos monster
    distance  =
      if fst (eCurrPos hero) > fst oldPos
      then
        (1,0)
      else
        (-1,0)

-- moves 1 tile away from player.
bhvMove1AwayFromPlayer :: World -> Entity -> Maybe World
bhvMove1AwayFromPlayer world monster = bhvMoveNTiles world monster distance
  where
    hero = wHero world
    oldPos = eCurrPos monster
    distance  =
      if fst (eCurrPos hero) > fst oldPos
      then
        (-1,0)
      else
        (1,0)
----------------------------------------

-- Fighter:
--------------------------------------
fighter = 
  MonsterType { mtName = "Fighter",
                mtExpRewardMultiplier = 1.0,
                mtHPMultiplier = 1.0,
                mtSpeedMultiplier = 1.0,
                
                mtHitDie = baseMonsterHitDie,
                mtEvadeDie = baseMonsterEvadeDie,
                mtDamageDie = baseMonsterDamageDie,
                mtMitigation = baseMonsterMitigation,
                
                mtBehaviorStack = [bhvMeleeCombat, bhvMove1TowardsPlayer, bhvWait]
              }
--------------------------------------
  
-- Crossbowman:
--------------------------------------
crossbowman = 
  MonsterType { mtName = "Crossbowman",
                mtExpRewardMultiplier = 1.0,
                mtHPMultiplier = 0.5,
                mtSpeedMultiplier = 2.0,
                
                mtHitDie = baseMonsterHitDie,
                mtEvadeDie = Dice {dDie = (1,10), dMod = -10},
                mtDamageDie = baseMonsterDamageDie,
                mtMitigation = -1,
                -- TODO: edit so that the crossbowman seeks to be in 2-4 range, not just to flee!
                mtBehaviorStack = [bhvCrossbowmanRangedAttack, bhvMove1AwayFromPlayer, bhvMeleeCombat, bhvWait]
              }
  
-- have these attributes as   
crossbowRange :: (Int, Int)
crossbowRange = (2, 4)

bhvCrossbowmanRangedAttack :: World -> Entity -> Maybe World
bhvCrossbowmanRangedAttack world monster
  | not playerInRange = Nothing -- player isn't in range, abort dat shit.
  | otherwise = Just $ simpleCombat monster hero world -- actual function here.
  where
    hero = wHero world
    playerInRange = entityInRange monster crossbowRange hero

  
baseMonsterDamageDie = Dice { dDie = (2, 2), dMod = 0 }
baseMonsterHitDie = Dice { dDie = (1, 20), dMod = 0 }
baseMonsterEvadeDie = Dice { dDie = (1, 20), dMod = 0 }
baseMonsterMitigation = 2



-- Helper functions:
--------------------------------------
-- instantaneous movement, no collision.
bhvMoveNTiles :: World -> Entity -> (Int, Int) -> Maybe World
bhvMoveNTiles world monster distance
  | not $ posIsClear desiredPosition world = Nothing
  | otherwise = 
    Just $ world {wLevel = lvl {lEntities = updateMap (eCurrPos monster) desiredMonster (lEntities lvl) } }
  where
    lvl = wLevel world
    
    oldPos = eCurrPos monster
    desiredPosition = distance |+| oldPos
    desiredMonster = monster { eCurrPos = desiredPosition,
                               eOldPos = oldPos
                             }
--------------------------------------