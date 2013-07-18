module Content.Base where

import qualified Data.Map as M

import Types.Common
import Types.Classes
import Types.Items
import Types.World

import Content.Boss
import Content.Classes
import Content.Races
import Content.StartingGear




baseWorld = World { wDepth = 0,
                    wHero = basePlayer,
                    wLevel = baseLevel,
                    wLevels = [baseLevel], 
                    wPrevInput = NoInput, 
                    wMessageBuffer = ["Welcome to Axis of X!"],
                    wStdGen = undefined,
                    wBoss = lastBoss,
                    wScreenShown = Skills
                }


baseLevel = Level { lDepth = 0,
                    lGold = M.empty,
                    lItems = M.empty,
                    lSize = 1,
                    lFloorTiles = M.empty,
                    lWallTiles = M.empty,                     
                    lEntities = M.empty }
            

basePlayer = Hero { hName = "Broseph",
                    hClass = peasant,
                    hRace = human,
                    hInventory = Inventory [] 0,
                    hLevel = 1,
                    hExperienceRemaining = 100,
                    hReputation = Asshole,
                    eCurrPos = (0,0),
                    eOldPos = (0,0),
                    eCurrHP = 10,
                    eMaxHP = 10,
                    hCurrEnergy = 50,
                    hMaxEnergy = 50,
                    eSpeed = 5,
                    eNextMove = 0,
                    hWield = shortSword,
                    hWear = rags,
                    hMovementSlack = (0, 9),
                    hViewDistance = 15, 
                    
                    
                    
                    eHitDie = baseHeroHitDie,
                    eDamageDie = baseHeroDamageDie,
                    eEvadeDie = baseHeroEvadeDie,
                    eMitigation = baseHeroMitigation
                    }  
            

baseHeroDamageDie = Dice { dDie = (2, 5), dMod = 0 }
baseHeroHitDie = Dice { dDie = (1, 20), dMod = 0 }
baseHeroEvadeDie = Dice { dDie = (1, 5), dMod = 10}
baseHeroMitigation = 2