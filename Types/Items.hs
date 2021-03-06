module Types.Items where


import Types.Common

-- ITEMS: Armor
-- (temp)
data Armor = Armor { aEvasion :: Int,
                     aMitigation :: Int,
                     aDescription :: String }
             
           deriving(Show, Eq)
                   
-- Items: Weapons
data WeaponQuality = JourneyMan 
                   | Master { wTitle :: String } 
                   | GrandMaster { wTitle :: String,
                                   wLegacy :: String }
                   deriving(Show, Eq)
                           


type WeaponRange   = Int


data Weapon = 
  Weapon { 
     -- These stats generate the "proper" stats for the weapon.
    wepQuality :: WeaponQuality,
    wepWeight :: WeaponWeight,
    wepType :: WeaponType,
    wepGrip :: WeaponGrip,    
    wepLevel :: Int,
     -- !
   
    
    
    -- "proper" stats:
    wepName :: String,
    wepDamageDie :: Dice,
    wepHitBonus :: Int,
    wepSpeedMultiplier :: Float,
    wepRange :: Int
     -- !
                
    }
  deriving (Eq)
           
instance Show Weapon where
  show w = filter (/= '\"') outStr
    where
      outStr = 
        case wepQuality w of
          JourneyMan -> 
            "Level " ++ show (wepLevel w) ++ " " ++ show (wepName w) ++ "\n" ++ 
            "(dmg: " ++ show (wepDamageDie w) ++ ", hitBonus: " ++ show (wepHitBonus w) ++ ", speedMult: " ++ show (wepSpeedMultiplier w) ++ ", range: " ++ show (wepRange w) ++ ")"
            
          Master title ->
            show title ++ ", the level " ++ show (wepLevel w) ++ " " ++ show (wepName w) ++ "\n" ++ 
            "(dmg: " ++ show (wepDamageDie w) ++ ", hitBonus: " ++ show (wepHitBonus w) ++ ", speedMult: " ++ show (wepSpeedMultiplier w) ++ ", range: " ++ show (wepRange w) ++ ")"

          GrandMaster title legacy ->
            show title ++ " - " ++ show legacy ++ ", the level " ++ show (wepLevel w) ++ " " ++ show (wepName w) ++ "\n" ++ "(dmg: " ++ show (wepDamageDie w) ++ ", hitBonus: " ++ show (wepHitBonus w) ++ ", speedMult: " ++ show (wepSpeedMultiplier w) ++ ", range: " ++ show (wepRange w) ++ ")"            





-- Items: Inventory
type Gold = Int

data Potion = Potion { pAmount :: Int,
                       pDesc :: String,
                       pEffect :: Effect } 
            deriving (Show, Eq)
                     

                           
data Item = Arm Armor | Pot Potion | Weap Weapon
          deriving(Show, Eq)
                     
data Inventory = Inventory [Item] Gold
               deriving(Show, Eq)