module Types.Items where


import Types.Common

-- ITEMS: Armor
-- (temp)
data Armor = Armor { aAvoidance :: Int,
                     aMitigation :: Int,
                     aDescription :: String }
             
           deriving(Show, Eq)
                   
-- Items: Weapons
data WeaponQuality = JourneyMan 
                   | Master { wTitle :: String } 
                   | GrandMaster { wTitle :: String,
                                   wLegacy :: String }
                   deriving(Show, Eq)
                           
data WeaponWeight  = Balanced | Heavy | Burdensome
                   deriving(Show, Eq)

data WeaponType    = Edged | Pointy | Blunt
                   deriving(Show, Eq)

data WeaponGrip    = OneHanded | TwoHanded
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
    wepDescription :: String,
    wepDamageDie :: Die,
    wepHitPenalty :: Int,
    wepSpeedMultiplier :: Float
     -- !
                
    }
  deriving(Show, Eq)

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