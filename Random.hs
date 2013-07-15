module Random where

import System.Random

import AxisData.Dice
-- select random element from list, undefined on empty lists.
randomListMember :: [a] -> StdGen -> (a, StdGen)
randomListMember [] _ = undefined
randomListMember as gen = (a, newGen)
  where
    (index, newGen) = randomR (0, (length as) - 1) gen
    a = as !! index
    
rollDie :: Dice -> StdGen -> (Int, StdGen)
rollDie dice gen = (result, lastGen)
  where
    (nofDie, maxDice) = dDie dice
    modifier = dMod dice
    
    generators = makeGeneratorList gen nofDie
    lastGen = last generators
    
    result = foldl (+) modifier $ map (\g -> fst $ randomR (1, maxDice) g) generators
    

makeGeneratorList :: StdGen -> Int -> [StdGen]
makeGeneratorList _ 0 = []
makeGeneratorList gen n = gen1:makeGeneratorList  gen2 (n-1)
  where
    (gen1, gen2) = split gen
    