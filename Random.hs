module Random (randomListMember, rollDie, makeGeneratorList) where


import System.Random

import Types.Common
import Types.World
import Types.Items
import Types.World

-- select random element from list, undefined on empty lists.
randomListMember :: [a] -> StdGen -> (a, StdGen)
randomListMember [] _ = error "Attempted to pick random member of an empty list in function \"randomListMember\""
randomListMember as gen = (a, newGen)
  where
    (index, newGen) = randomR (0, (length as) - 1) gen
    a = as !! index
    
-- Roll a dice or multiple die.
rollDie :: Dice -> StdGen -> (Int, StdGen)
rollDie dice gen = (result, lastGen)
  where
    (nofDie, maxDice) = dDie dice
    modifier = dMod dice
    
    generators = makeGeneratorList gen nofDie
    lastGen = last generators
    
    roll = foldl (+) modifier $ map (\g -> fst $ randomR (1, maxDice) g) generators
    
    result = max roll 0 -- no negative rolls if  negative modifiers + low rolls.
    

-- splits a generator into n generators.
makeGeneratorList :: StdGen -> Int -> [StdGen]
makeGeneratorList _ 0 = []
makeGeneratorList gen n = gen1:makeGeneratorList  gen2 (n-1)
  where
    (gen1, gen2) = split gen
    
    
    
