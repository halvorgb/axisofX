module AxisData.Items.Weapons where


import AxisData.Dice

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
    
               
    