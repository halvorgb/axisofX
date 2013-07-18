module Content.Boss where

import Types.World
import Types.Common


lastBoss = Boss {  bName = "Kitty the Just",
                   bInnocentKills = 0,
                   bRivalKills = 0,
                   
                   eCurrHP = 100,
                   eMaxHP = 100,
                   
                   eCurrPos = (-200, 0),
                   eOldPos = (-200, 0),
                   eSpeed = 20,
                   eNextMove = 20,
                   
                   eHitDie = baseMonsterHitDie,
                   eDamageDie = baseMonsterDamageDie,
                   eEvadeDie = baseMonsterEvadeDie,
                   eMitigation = baseMonsterMitigation
                                 
                                             
                }
           
           
baseMonsterDamageDie = Dice { dDie = (2, 2), dMod = 0 }
baseMonsterHitDie = Dice { dDie = (1, 20), dMod = 0 }
baseMonsterEvadeDie = Dice { dDie = (1, 20), dMod = 15 }
baseMonsterMitigation = 0          