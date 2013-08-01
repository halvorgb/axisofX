module ItemGeneration(generateWeapon, generateWeaponStats) where

import System.Random

import Types.Items
import Types.Common

import Content.Names

generateWeapon :: Int -> StdGen -> (Weapon, StdGen)
generateWeapon wpLevel gen = (generateWeaponStats wpWeight wpType wpGrip wpLevel' wpQuality, gen'''')
                                    
  where
    (wpWeight, gen') = random gen
    (wpType, gen'') = random gen'
    (wpGrip, gen''') = random gen''
    (wpQuality, gen'''') = generateWeaponQuality wpType wpGrip gen'''
    
    wpLevel' =
      case wpQuality of 
        JourneyMan -> wpLevel
        Master { } -> wpLevel + 4
        GrandMaster { } -> wpLevel + 8


generateWeaponStats :: WeaponWeight -> WeaponType -> WeaponGrip -> Int -> WeaponQuality -> Weapon
generateWeaponStats wpWeight wpType wpGrip wpLevel wpQuality =
  Weapon {
    wepQuality = wpQuality,
    wepWeight = wpWeight,
    wepType = wpType,
    wepGrip = wpGrip,
    wepLevel = wpLevel,
    
    wepName = getName wpGrip wpType wpWeight,
    wepDamageDie = calcDmgDie wpGrip wpType wpWeight wpLevel,
    wepHitBonus = calcHitBonus wpGrip wpType wpWeight,
    wepSpeedMultiplier = calcSpeedMult wpGrip wpType wpWeight,
    wepRange = calcRange wpGrip wpType wpWeight
    }

generateWeaponQuality :: WeaponType -> WeaponGrip -> StdGen -> (WeaponQuality, StdGen)
generateWeaponQuality wepType wepGrip gen
  | randN < 71 = (JourneyMan, gen')
  | randN < 91 = (Master {wTitle = title}, gen'')
  | otherwise = (GrandMaster {wTitle = title, wLegacy = legacy}, gen''')
  where
    -- random distribution 1-100: 1-70 = normal, 71-90 = master, 91:100 = grandmaster.
    (randN, gen') = randomR ((1, 100) :: (Int, Int)) gen
    (title, gen'') = randomWeaponName wepType wepGrip gen'
    (legacy, gen''') = randomWeaponLegacy gen''
    
    
    

calcDmgDie :: WeaponGrip -> WeaponType -> WeaponWeight -> Int -> Dice
calcDmgDie wpGrip wpType wpWeight wpLevel =
  case wpType of
    Blunt -> case wpGrip of
      OneHanded -> case wpWeight of
        Balanced   -> Dice (1, 6) dmgMod
        Heavy      -> Dice (1, 8) dmgMod
        Burdensome -> Dice (1, 10) dmgMod
      TwoHanded -> case wpWeight of
        Balanced   -> Dice (1, 8) dmgMod
        Heavy      -> Dice (1, 10) dmgMod
        Burdensome -> Dice (1, 12) dmgMod
        
    Edged -> case wpGrip of
      OneHanded -> case wpWeight of
        Balanced   -> Dice (1, 5) dmgMod
        Heavy      -> Dice (1, 7) dmgMod
        Burdensome -> Dice (1, 9) dmgMod
      TwoHanded -> case wpWeight of
        Balanced   -> Dice (1, 7) dmgMod
        Heavy      -> Dice (1, 9) dmgMod
        Burdensome -> Dice (1, 11) dmgMod
          
    Pointy -> case wpGrip of
      OneHanded -> case wpWeight of
        Balanced   -> Dice (1, 4) dmgMod
        Heavy      -> Dice (1, 6) dmgMod
        Burdensome -> Dice (1, 8) dmgMod
      TwoHanded -> case wpWeight of
        Balanced   -> Dice (1, 6) dmgMod
        Heavy      -> Dice (1, 8) dmgMod
        Burdensome -> Dice (1, 10) dmgMod 
  where
    dmgMod = (mod 2 wpLevel) - 1
      
            


calcHitBonus :: WeaponGrip -> WeaponType -> WeaponWeight -> Int
calcHitBonus wpGrip wpType wpWeight = 
   case wpGrip of
        OneHanded -> case wpType of
          Blunt -> case wpWeight of
            Balanced -> 2
            Heavy -> 1
            Burdensome -> 0
            
          Edged -> case wpWeight of
            Balanced -> 3
            Heavy -> 2
            Burdensome -> 1
              
          Pointy -> case wpWeight of
            Balanced -> 4
            Heavy -> 3
            Burdensome -> 2
              
              
        TwoHanded -> case wpType of
          Blunt -> case wpWeight of
            Balanced -> -1
            Heavy -> -2
            Burdensome -> -3
              
          Edged -> case wpWeight of
            Balanced -> 0
            Heavy -> -1
            Burdensome -> -2
          Pointy -> case wpWeight of
            Balanced -> 1
            Heavy -> 0
            Burdensome -> -1
            
            
            
calcRange :: WeaponGrip -> WeaponType -> WeaponWeight -> Int
calcRange wpGrip wpType wpWeight =
  case wpGrip of
    OneHanded -> case wpType of
      Edged -> 1
      Pointy -> 2
      Blunt -> 1
    TwoHanded -> case wpType of
      Edged -> case wpWeight of
        Balanced -> 3
        Heavy -> 2
        Burdensome -> 2
      Pointy -> case wpWeight of
        Balanced -> 3
        Heavy -> 3
        Burdensome -> 2
      Blunt -> case wpWeight of
        Balanced -> 3
        Heavy -> 2
        Burdensome -> 1
        
calcSpeedMult :: WeaponGrip -> WeaponType -> WeaponWeight -> Float
calcSpeedMult _ _ wpWeight =
  case wpWeight of
    Balanced -> 1.0
    Heavy -> 1.25
    Burdensome -> 1.5


getName :: WeaponGrip -> WeaponType -> WeaponWeight -> String
getName wpGrip wpType wpWeight =
  case wpType of
    Edged ->
      case wpGrip of
        OneHanded ->
          case wpWeight of
            Balanced -> "Short Sword"
            Heavy -> "Hatchet"
            Burdensome -> "Battle Axe"
        
        TwoHanded ->
          case wpWeight of 
            Balanced -> "Flamberge"
            Heavy -> "Claymore"
            Burdensome -> "Executioner's Sword"
    
    Pointy ->
      case wpGrip of
        OneHanded ->
          case wpWeight of
            Balanced -> "Stiletto"
            Heavy -> "Rapier"
            Burdensome -> "Spear"
        
        TwoHanded ->
          case wpWeight of 
            Balanced -> "Half Pike"
            Heavy -> "Pike"
            Burdensome -> "Trident"
    
    Blunt ->
      case wpGrip of
        OneHanded ->
          case wpWeight of
            Balanced -> "Club"
            Heavy -> "War Hammer"
            Burdensome -> "Flail"
        
        TwoHanded ->
          case wpWeight of 
            Balanced -> "Great Club"
            Heavy -> "Bec de Corbin"
            Burdensome -> "Meteor Hammer"

    
    





