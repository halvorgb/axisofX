module Content.MonsterTypes where

import Types.Monsters
import Types.Common

monsterTypes = [fighter]



fighter = 
  MonsterType { mtName = "Berserker",
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
baseMonsterEvadeDie = Dice { dDie = (1, 20), dMod = 15 }
baseMonsterMitigation = 0