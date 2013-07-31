module Types.Common where

import System.Random

type Position = (Int, Int)


data Input = Dir Direction | Exit | Wait | Rest | NoInput | Show Screen
           | Queue Int | ExecuteSkills
           deriving (Eq, Show)

data Direction = Left | Right 
               deriving (Eq, Show)
                        

                        
data Screen = Skills | Inv | Help | LevelUp | Console
            deriving (Eq, Show)
                        
data HitMask = Allies | Enemies | All
             deriving (Show, Eq)
                      
                      
data Effect = Heal Int | Harm Int
            deriving(Show, Eq)
                   
-- which skills an item or class allows.
-- By category?
---
data SkillMask = Brute | Clever  | Finesse | Shady 
               deriving(Bounded, Eq, Enum, Ord, Show)                       

skillMaskMaxBound :: Int 
skillMaskMaxBound = fromEnum (maxBound :: SkillMask)

anySkillMask :: [SkillMask]
anySkillMask = enumFrom Brute
---

class ShowLong a where
  showLong  :: a -> String
                                          
-- TODO: are these used?
data Stat = Stat_HP | Stat_NRG | Stat_Hit | Stat_Dmg | Stat_Evd | Stat_Mit | Stat_Spd | Stat_Pos
          deriving (Show, Eq)

data SkillRange = SRWeaponRange
                | SRConst Int
                deriving (Eq)

data WeaponConstraints = WeaponConstraints { wcWeight  :: [WeaponWeight],
                                             wcType    :: [WeaponType],
                                             wcGrip    :: [WeaponGrip]
                                           }
                         deriving (Eq)

data WeaponWeight  = Balanced | Heavy | Burdensome
                   deriving(Show, Bounded, Enum, Eq)

instance Random WeaponWeight where
  random g = case randomR (fromEnum (minBound :: WeaponWeight), fromEnum (maxBound :: WeaponWeight)) g of
    (r, g') -> (toEnum r, g')
    
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')
  
data WeaponType    = Edged | Pointy | Blunt
                   deriving(Show, Bounded, Enum, Eq)
                           
instance Random WeaponType where
  random g = case randomR (fromEnum (minBound :: WeaponType), fromEnum (maxBound :: WeaponType)) g of
    (r, g') -> (toEnum r, g')
    
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')


data WeaponGrip    = OneHanded | TwoHanded
                   deriving(Show, Bounded, Enum, Eq)
                           
instance Random WeaponGrip where
  random g = case randomR (fromEnum (minBound :: WeaponGrip), fromEnum (maxBound :: WeaponGrip)) g of
    (r, g') -> (toEnum r, g')
    
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
    (r, g') -> (toEnum r, g')


{-
+instance Random MonsterType where
+    random g = case randomR (fromEnum (minBound :: MonsterType), fromEnum (maxBound :: MonsterType)) g of
+                 (r, g') -> (toEnum r, g')
+    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
+                        (r, g') -> (toEnum r, g')
-}                       
                    
                    
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
  show d
     | modifier > 0 =  
       show nofDie ++ "d" ++ show maxDice ++ " + " ++ show modifier
     | modifier < 0 = 
       show nofDie ++ "d" ++ show maxDice ++ " - " ++ show (abs modifier)
     | otherwise =
         show nofDie ++ "d" ++ show maxDice
     where
       (nofDie, maxDice) = dDie d
       modifier = dMod d