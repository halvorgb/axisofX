module Types.Tiles where

import System.Random

data WallTile = Door | Wall
              deriving (Show, Bounded, Enum, Eq)

data FloorTile = Grass | Water 
               deriving (Show, Bounded, Enum, Eq)

instance Random FloorTile where
    random g = case randomR (fromEnum (minBound :: FloorTile), fromEnum (maxBound :: FloorTile)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')