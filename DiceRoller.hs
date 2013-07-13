module DiceRoller where

import System.Random

import AxisData.Dice


rollDie :: StdGen -> Dice -> (Int, StdGen)
rollDie gen dice = (result, lastGen)
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