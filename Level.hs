module Level where

import qualified Data.Map as M
import Data.Maybe

import Types

isWater coord lvl = case M.lookup coord (lFloorTiles lvl) of
  Just Water -> True
  _ -> False

isDoor coord lvl = case M.lookup coord (lWallTiles lvl) of
  Just Door -> True
  _ -> False

isGrass coord lvl = case M.lookup coord (lFloorTiles lvl) of
  Just Grass -> True
  _ -> False

isGold coord lvl = M.member coord (lGold lvl)

isMonster coord lvl = any (\x -> case x of 
                              Monster { } -> True
                              _ -> False
                          ) resList
  where
    resList = fromMaybe [] $ M.lookup coord (lEntities lvl)


isArmor coord lvl =  any (\x -> case x of 
                             Arm { } -> True
                             _ -> False
                          ) resList
  where
    resList = fromMaybe [] $ M.lookup coord (lItems lvl)

  
  



isPotion coord lvl = any (\x -> case x of 
                             Pot { } -> True
                             _ -> False
                          ) resList
  where
    resList = fromMaybe [] $ M.lookup coord (lItems lvl)

isWeapon coord lvl =  any (\x -> case x of 
                             Weap { } -> True
                             _ -> False
                          ) resList
  where
    resList = fromMaybe [] $ M.lookup coord (lItems lvl)


-- og lignende for alle andre funksj.

{-

map1 = [
  "                                                                                       ",
  "~~~~~~~~~~~~~~~~~~~~##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################~~~~" ]
map2 = [ "       ",
         "#######"]
level1 = strToLevel map1
-}