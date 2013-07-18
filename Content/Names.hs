module Content.Names where

import System.Random
import Random

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
           
randomName :: StdGen -> String
randomName gen = pre ++ suff
  where
    (pre, gen') = randomListMember prefixes gen
    (suff, _) = randomListMember suffixes gen'
  
           
