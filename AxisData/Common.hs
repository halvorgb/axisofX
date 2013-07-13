module AxisData.Common where


-- move this, incorporate classes/races into it.
 -- i.e. classes and races have experience penalties etc.
calculateExperience :: Int -> Int -- Level -> experience required.
calculateExperience level = experience
  where
    experience = 100 * level;
    
    
    
type Position = (Int, Int)


data Input = Dir Direction | Exit | Wait | NoInput | Show Screen
           deriving (Eq, Show)

data Direction = Left | Right 
               deriving (Eq, Show)
                        
data Screen = Skills | Inv | Help | LevelUp
            deriving (Eq, Show)
                        
data HitMask = Allies | Enemies | All
             deriving (Show, Eq)
                      
                      
data Effect = Heal Int | Harm Int
            deriving(Show, Eq)