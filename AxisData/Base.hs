module AxisData.Base where

import qualified Data.Map as M

import AxisData.Entities
import AxisData.World
import AxisData.Common
import AxisData.Classes
import AxisData.Dice
import AxisData.Items.Inventory
import AxisData.Items.Armor
import AxisData.Items.Weapons
import AxisData.Skills



baseWorld = World { wDepth = 0,
                    wHero = basePlayer,
                    wLevel = baseLevel,
                    wLevels = [baseLevel], 
                    wPrevInput = NoInput, 
                    wMessageBuffer = ["Welcome to Axis of X!"],
                    wStdGen = undefined,
                    wBoss = baseBoss,
                    wScreenShown = Skills
                }


baseLevel = Level { lDepth = 0,
                    lGold = M.empty,
                    lItems = M.empty,
                    lSize = 1,
                    lFloorTiles = M.empty,
                    lWallTiles = M.empty,                     
                    lEntities = M.empty }
             
             
baseMonster = Monster { mType = Noble,
                        mRace = Ogre,
                        mInventory = Inventory [] 0,
                        mLevel = 0, 
                        mExperience = 1, 
                        mID = 1,
                        eCurrPos = (0,0),
                        eOldPos = (0,0),
                        mCurrHP = 1,
                        mMaxHP = 1,
                        eSpeed = 10,
                        eNextMove = 10 }



baseBoss = Boss {  bName = "Kitty the Just",
                   bInnocentKills = 0,
                   bRivalKills = 0,
                   bCurrHP = 100,
                   bMaxHP = 100,
                   
                   eCurrPos = (-200, 0),
                   eOldPos = (-200, 0),
                   eSpeed = 20,
                   eNextMove = 20
               
                }

basePlayer = Hero { hName = "Broseph",
                    hClass = peasant,
                    hRace = Hobgoblin,
                    hInventory = Inventory [] 0,
                    hLevel = 1,
                    hExperienceRemaining = 100,
                    eCurrPos = (0,0),
                    eOldPos = (0,0),
                    hCurrHP = 10,
                    hMaxHP = 10,
                    hCurrEnergy = 50,
                    hMaxEnergy = 50,
                    eSpeed = 5,
                    eNextMove = 0,
                    hWield = baseShortSword,
                    hWear = baseRags,
                    hMovementSlack = (0, 9),
                    hViewDistance = 15 }
                        
             
             
-- Starting weapons: Not randomly generated.
baseShortSword = 
  Weapon {
    wepQuality = JourneyMan,
    wepWeight = Balanced,
    wepType = Edged,
    wepGrip = OneHanded,
    wepLevel = 1,
    
    wepDescription = "Dull Short Sword (dmg: 1d6, hitPenalty: (-0), spdMultiplier: 1.0), level 1 crafted by a Journeyman.",
    wepDamageDie = (1, 6),
    wepHitPenalty = 0,
    wepSpeedMultiplier = 1.0
    }

baseRags = Armor { aAvoidance = 0,
                   aMitigation = 0,
                   aDescription = "RAGS!"
                 }
           


-- Classes?...
peasant = Class { cName = "Peasant",
                  cExpReqMultiplier = 1.0,
                  cStartingHP = 10,
                  cHPGain = 1,
                  cStartingEnergy = 50,
                  cEnergyGain = 5,
                  
                  cWield = baseShortSword,
                  cWear = baseRags,
                  cInventory = Inventory [] 0,
                  
                  cReputation = Asshole
                }
            