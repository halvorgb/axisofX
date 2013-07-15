module AxisData.Entities where


import System.Random


import AxisData.Common
import AxisData.Items.Inventory
import AxisData.Items.Armor
import AxisData.Items.Weapons
import AxisData.Classes

import AxisData.Dice



data Entity = Monster { mType :: MonsterType,
                        mRace :: Race,
                        mInventory :: Inventory,
                        mLevel :: Int, 
                        mExperience :: Int, 
                        mCurrHP :: Int,
                        mMaxHP :: Int,
                        mID :: Int, -- to make each monster unique
                        
                        --common...                        
                        eSpeed :: Int, -- How much time between each action.
                        eNextMove :: Int, -- How much time until the NEXT action.                        
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        
                        eHitDie :: Dice,
                        eDamageDie :: Dice,
                        eEvadeDie :: Dice,
                        eMitigation :: Int
                        
                      }
                  
            | Hero    { hName :: String,  
                        hClass :: Class,
                        hRace :: Race,
                        hInventory :: Inventory,
                        hLevel :: Int,
                        hExperienceRemaining :: Int,
                        hCurrHP :: Int,
                        hMaxHP :: Int,
                        hCurrEnergy :: Int,
                        hMaxEnergy :: Int,
                        hWield :: Weapon,
                        hWear :: Armor,
                        hMovementSlack :: (Int, Int),  -- the coordinates that the hero can move between without wrapping.
                        hViewDistance :: Int, -- Added to $ snd hMovementSlack
                        
                        --common for all entities. Duplicated for ease of use.
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        
                        eSpeed :: Int,
                        eNextMove :: Int,
                        
                        eHitDie :: Dice, -- updated on gear changes.
                        eDamageDie :: Dice,
                        eEvadeDie :: Dice,
                        eMitigation :: Int
                        } 
            | Boss    { bName :: String,
                        bInnocentKills :: Int,
                        bRivalKills :: Int,
                        bCurrHP :: Int,
                        bMaxHP :: Int,
                        
                        eCurrPos :: Position,
                        eOldPos :: Position,
                        eSpeed :: Int,
                        eNextMove :: Int,
                        
                        eHitDie :: Dice,
                        eDamageDie :: Dice,
                        eEvadeDie :: Dice,
                        eMitigation :: Int                        
                        }
            deriving (Eq)

instance Show Entity where
    show e = filter (/= '\"') outString -- remove them silly "'s.
      where
        outString = case e of
          Hero {} -> (show $ hName e) ++ " the " ++ (show $ hRace e) ++ " " ++ (show $ hClass e)
          Monster {} -> (show $ mRace e) ++ " " ++ (show $ mType e) ++ "[" ++ (show $ mLevel e) ++ "]"
          Boss {} -> show $ bName e
      

data Race = Ogre | Giant | Troll | Orc | Goblin | Hobgoblin
          deriving (Show, Bounded, Enum, Eq, Read)
               
data MonsterType = Politician | Noble 
                 deriving (Show, Bounded, Enum, Eq)
                 
instance Random MonsterType where
    random g = case randomR (fromEnum (minBound :: MonsterType), fromEnum (maxBound :: MonsterType)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')



             