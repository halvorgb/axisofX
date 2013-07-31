module Content.Names where

import System.Random
import Random

import Types.Common

prefixes = [
  "Ack",
  "Bur",
  "Kek",
  "Bro",
  "Fry",
  "Pio",
  "Aga",
  "Tro",
  "Orph",
  "Myll",
  "Jjij"
  ]
           
suffixes = [
  "bar",
  "bur",
  "bier",
  "mann",
  "sson",
  "sen",
  "vik",
  "rukk",
  "stein",
  "zax",
  "stadt",
  "le",
  "gma",
  "ion",
  "elus"
  ]
           
randomHeroName :: StdGen -> String
randomHeroName gen = pre ++ suff
  where
    (pre, gen') = randomListMember prefixes gen
    (suff, _) = randomListMember suffixes gen'
    
    
  
-- WeaponNames:
pointyOneHandedNames =
  [
    "Needle",
    "Gutter"
  ]
pointyTwoHandedNames =
  [
    "Skewer",
    "Spitter"
  ]
edgedOneHandedNames =
  [
    "Slice",
    "The Black Sword"
  ]
edgedTwoHandedNames =
  [
    "Ice",
    "The Zweihander"
  ]
  
bluntOneHandedNames =
  [
    "Clobber",
    "The Skull Flail",
    "Dinosaur Bone"
  ]
  
bluntTwoHandedNames =
  [
    "Lung Collapser",
    "Face Surgeon",
    "Knee Crusher"
  ]
  
randomWeaponName :: WeaponType -> WeaponGrip -> StdGen -> (String, StdGen)
randomWeaponName weaponType weaponGrip gen = randomListMember weaponNameList gen
  where
    weaponNameList = 
      case weaponType of
        Edged -> case weaponGrip of
          OneHanded -> edgedOneHandedNames
          TwoHanded -> edgedTwoHandedNames
        Blunt -> case weaponGrip of
          OneHanded -> bluntOneHandedNames
          TwoHanded -> bluntTwoHandedNames
        Pointy -> case weaponGrip of
          OneHanded -> pointyOneHandedNames
          TwoHanded -> pointyTwoHandedNames

  
legacyPrefix = 
  [
    "Slayer of ",
    "Devourer of ",
    "Savior of ",
    "Drinker of ",
    "Drenched in ",
    "Previously owned by "
  ]

legacySuffix =
  [
    "virgins",
    "widows",
    "heroes",
    "fools",
    "yeastmongers",
    "nobles"
  ]

randomWeaponLegacy :: StdGen -> (String, StdGen)
randomWeaponLegacy gen = (pre ++ suff, gen'')
  where
    (pre, gen') = randomListMember legacyPrefix gen
    (suff, gen'') = randomListMember legacySuffix gen'
    


           
