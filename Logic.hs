module Logic(createHero, think) where

import Prelude hiding (Either(..))
import qualified Data.Map as M
import Data.Maybe
import Data.List

import Types.World
import Types.Common
import Types.Items

import AI
import Random
import Helpers




-- First time calculation of hero stats
createHero :: World -> String -> Race -> Class -> World
createHero w n r c  = 
  w { wHero = h {
         hName = n,
         hClass = c,
         hRace = r,
         hInventory = inventory,
         hReputation = reputation,
         hSkills = skills,
         hCurrEnergy = energy,
         hMaxEnergy = energy,
         hWield = weapon,
         hWear = armor,
         eCurrHP = hp,
         eMaxHP = hp,
         
         eHitDie = hitDie,
         eDamageDie = dmgDie,
         eEvadeDie = evdDie,
         eMitigation = mitigation
         }
    }
                      
  where
    h = wHero w
    
    inventory = cStartingInventory c   
    -- todo: experience    
    reputation = cStartingReputation c    
    energy = round $ (fromIntegral $ (rBaseEnergy r)) * (cStartingEnergyMultiplier c)    
    skills = cStartingSkills c
    hp = round $ (fromIntegral $ (rBaseHP r)) * (cStartingHPMultiplier c)
    
    armor = cStartingArmor c
    weapon = cStartingWeapon c
    
    hitDie = (cHitDie c) { dMod = (dMod $ cHitDie c) + (rHitModifier r) + (wepHitBonus weapon) }
    evdDie = (cEvadeDie c) { dMod = (dMod $ cEvadeDie c) + (rEvasionModifier r) + (aEvasion armor)}
    mitigationMod = (cMitigationBonus c) + (rMitigationModifier r)
    damageMod = (cDamageBonus c) + (rDamageModifier r)
    
    dmgDie = (wepDamageDie weapon) { dMod = (dMod $ wepDamageDie weapon) + damageMod}
    mitigation = mitigationMod + (aMitigation armor)
    
    
    

-- Finds which entities whose turns are up, performs AI, those entities whose turns are not up have updated their eNextMove values. Returns a new World with these changes.
think :: World -> IO World
think world = do
  return world''
    where
      h = wHero world
      lvl = wLevel world
      
      e = h:(getEntitiesFromViewFrame world $ getViewFrame world) -- every entity in view       
      
      e' = prepare e -- subtracts the lowest eNextMove value from every entity,
      -- always yielding 1 with 0.
      
      
      h' = fromJust $ find (\x -> case x of  -- find to not have to iterate through the whole thing, fromJust is safe because the hero was added to e.
                               Hero { } -> True                         --  Crashing is a good idea if hero can't be found in e'
                               _ -> False
                           ) e'
      
      e'' =  filter (\x -> (eNextMove x == 0))  e' -- the monsters whose turns are up!
      -- monsters whose turns are now
      monstersAI    = filter monsterFilter e''
      -- monsters whose turns are not up yet.
      monstersWAIT  = (e' \\ [h']) \\ monstersAI
      
      monsterFilter :: Entity -> Bool
      monsterFilter x = case x of
        Monster {} -> True
        _ -> False
        
      -- update the map for monsters whose turns are not yet up!
      emap = foldl' (\map m -> updateMap (eCurrPos m) m map) (lEntities lvl) monstersWAIT
      world' = world { wLevel = lvl {lEntities = emap }, wHero = h'}
      
      -- choose an action for the rest of the monsters, execute it, reset time until next move.
      monsters' = map (\m -> m {eNextMove = eSpeed m}) monstersAI
      world'' =  foldl' (\w m -> performAI m w) world' monsters'

      
       

-- find the lowest timeRemaining, subtract it from all.
prepare :: [Entity] -> [Entity]
prepare e = map (\x -> x { eNextMove = eNextMove x - lowestRemaining }) e
  where
    -- 10000 is infinite in this case.
    lowestRemaining = foldl' (\x y -> min x $ eNextMove y) 10000 e


