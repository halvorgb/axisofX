module Types.Common where

import System.Random

type Position = (Int, Int)


data Input = Dir Direction | Exit | Wait | NoInput | Show Screen
           deriving (Eq, Show)

data Direction = Left | Right 
               deriving (Eq, Show)
                        
data Screen = Skills | Inv | Help | LevelUp
            deriving (Eq, Show)
                        
data HitMask = Allies | Enemies | All
             deriving (Show, Eq)
                      
                      
data Effect = Heal Int | Harm Int
            deriving(Show, Eq)
                   
-- which skills an item or class allows.
-- By category?
data SkillMask = Brute | Finesse | Common | Clever | Shady 
               deriving(Show, Eq)
                       
data WeaponConstraints = WeaponConstraints { wcWeight  :: [WeaponWeight],
                                             wcType    :: [WeaponType],
                                             wcGrip    :: [WeaponGrip]
                                           }
                         deriving (Eq)
                                  
                                  
data WeaponWeight  = Balanced | Heavy | Burdensome
                   deriving(Show, Eq)

data WeaponType    = Edged | Pointy | Blunt
                   deriving(Show, Eq)

data WeaponGrip    = OneHanded | TwoHanded
                   deriving(Show, Eq)
                       
                    
                    
--Dice:
type NOFDie = Int
type MaxDice = Int
type Die = (NOFDie, MaxDice)

type Modifier = Int

data Dice = 
  Dice {
    dDie :: Die, 
    dMod :: Modifier
    }
  deriving(Eq)
          
instance Show Dice where
  show d = (show nofDie) ++ "d" ++ (show maxDice) ++ " + " ++ (show modifier)
    where
      (nofDie, maxDice) = dDie d
      modifier = dMod d