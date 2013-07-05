module Render.Console.Layout where

dimensions :: (Int, Int)
dimensions = (100,26)

viewPortPosition :: (Int, Int) -- top left corner
viewPortPosition = (0,0) -- change this.

-- other variables needed: positions for every value that is to be changed.



baseLayout :: [String]
baseLayout = 
  [
    "// <Name> the level <Level> <Race> <Class>, on level <DungeonLevel>.                                .",
    "                                                                                                    .",
    "                                                                                                    .", -- <-- viewport wall
    "                                                                                                    .", -- <-- viewport floor
    "                                                                                                    .",
    "                                                                         ________                   .",
    " _ _ _ _ _ _ _ _     _______      _______      _______      _______     |        |  Spd penalty:    .",
    "| HP: xxx/yyy   |   |1      |    |2      |    |3      |    |4      |    |_ Enter |   <p>%           .",
    "| EN: xxx/yyy   |   | <skl> | -> | <skl> | -> | <skl> | -> | <skl> | ->   |  <-^ |  E. Cost:        .",
    "|               |   |_______|    |_______|    |_______|    |_______|      |      |   <num>          .",
    "| STR: a        |                                                         |______|                  .",
    "| DEX: b        |                                                                                   .",
    "| CON: c        |                                                                                   .",
    "| INT: d        |                                                                                   .", -- combat log etc.
    "| WIS: e        |                                                                                   .",
    "| CHA: f        |                                                                                   .",
    "|               |                                                                                   .",
    "| <effects>     |                                                                                   .",
    "|               |                                                                                   .",
    " - - - - - - - -                                                                                    .",
    "                                                                                                    .",
    "                                                                                                    .",
    "                                                                                                    .",
    "                                                                                                    .",
    "                                                                                                    .",
    "                                                                                                    .",
    "....................................................................................................."
  ]
