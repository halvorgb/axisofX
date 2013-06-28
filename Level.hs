module Level where

import qualified Data.Map as M

import Types

isWater coord lvl = case M.lookup coord (lTiles lvl) of
  Just Water -> True
  _ -> False

isDoor coord lvl = case M.lookup coord (lTiles lvl) of
  Just Door -> True
  _ -> False

isFloor coord lvl = case M.lookup coord (lTiles lvl) of
  Just Floor -> True
  _ -> False

isGold coord lvl = M.member coord (lGold lvl)

isMonster coord lvl = case M.lookup coord (lEntities lvl) of 
  Just ((Entity _ _ (Monster _ _ _) _ _):_) -> True
  _ -> False


isArmor coord lvl = case M.lookup coord (lItems lvl) of
  Just ((Arm _):_) -> True
  _ -> False

isPotion coord lvl = case M.lookup coord (lItems lvl) of
  Just ((Pot _):_) -> True
  _ -> False
  
isWeapon coord lvl = case M.lookup coord (lItems lvl) of
  Just ((Weap _):_) -> True
  _ -> False




-- og lignende for alle andre funksj.

{-

map1 = [
  "                                                                                       ",
  "~~~~~~~~~~~~~~~~~~~~##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################~~~~" ]
map2 = [ "       ",
         "#######"]
level1 = strToLevel map1
-}