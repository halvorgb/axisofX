module Level where

import qualified Data.Map as M

import Types


strToLevel :: [String] -> Level
strToLevel str = foldl populate emptyLevel {lSize = maxX} asciiMap
  where
    asciiMap = concat $ zipWith zip coords str
    coords = [[(x,y) | x <- [0..]] | y <- [0..]]
    maxX = maximum . map (fst . fst ) $ asciiMap
    populate lvl (coord, tile) =
      case tile of
        '#' -> lvl { lTiles = M.insert coord Floor t }
        '~' -> lvl { lTiles = M.insert coord Water t }
        '|' -> lvl { lTiles = M.insert coord Door t }
--        '\\' -> lvl { lTiles = M.insert coord Stairs t }
        _ -> lvl
        where
          t = lTiles lvl
          
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
isVillain coord lvl = M.member coord (lVillains lvl)

isArmor coord lvl = case M.lookup coord (lItems lvl) of
  Just (Arm _) -> True
  _ -> False

isPotion coord lvl = case M.lookup coord (lItems lvl) of
  Just (Pot _) -> True
  _ -> False
  
isWeapon coord lvl = case M.lookup coord (lItems lvl) of
  Just (Weap _) -> True
  _ -> False




-- og lignende for alle andre funksj.



map1 = [
  "                                                                                       ",
  "~~~~~~~~~~~~~~~~~~~~##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~######################~~~~" ]
map2 = [ "       ",
         "#######"]
level1 = strToLevel map1