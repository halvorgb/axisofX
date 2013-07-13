module AxisData.Common where


-- move this, incorporate classes into it.
calculateExperience :: Int -> Int -- Level -> experience required.
calculateExperience level = experience
  where
    experience = 100 * level;
    
    
    
type Position = (Int, Int)


data Input = Dir Direction | Exit | Wait | NoInput
           deriving (Eq, Show)

data Direction = Left | Right 
               deriving (Eq, Show)
                        
                        
data HitMask = Allies | Enemies | All
             deriving (Show, Eq)