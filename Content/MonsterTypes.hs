module Content.MonsterTypes where

import Types.MonsterTypes
import Types.Common

monsterTypes = [fighter]



fighter = 
  MonsterType { mtName = "Fighter",
                mtExpRewardMultiplier = 1.0,
                mtHPMultiplier = 1.0,
                mtSpeedMultiplier = 1.0,
                
                mtHitDie = baseMonsterHitDie,
                mtEvadeDie = baseMonsterEvadeDie,
                mtDamageDie = baseMonsterDamageDie,
                mtMitigation = baseMonsterMitigation
              }
  
baseMonsterDamageDie = Dice { dDie = (2, 2), dMod = 0 }
baseMonsterHitDie = Dice { dDie = (1, 20), dMod = 0 }
baseMonsterEvadeDie = Dice { dDie = (1, 20), dMod = 0 }
baseMonsterMitigation = 2