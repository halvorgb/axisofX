module Render.SDL.Text (FontAssets, loadFont, drawText) where

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL

import Types

fontFilePath = "assets/fonts/OldStandard-Regular.ttf"

bottomPos :: Position
bottomPos = (48, 560)

type FontAssets = Font


loadFont :: IO FontAssets
loadFont = do
  lol <- TTFG.init
  font <- openFont fontFilePath 14
  return font
  

drawText :: World -> SDL.Surface -> Font -> IO ()
drawText world mainSurf font = do
  let surfaces = createSurfaces font messageBuffer (Color 10 10 20)
  let preppedOutput = zip positions surfaces
  
--  renderText mainSurf ((30, 30), renderTextSolid font (head messageBuffer) (Color 255 255 255))
  
  mapM_ (renderText mainSurf)  preppedOutput
  
  where
    messageBuffer = wMessageBuffer world
    
    positions :: [Position]
    positions = createPositions bottomPos (length messageBuffer) (-16)
      
      --reverse [(fst bottomPos, y) | y <- [(snd topPos)..(snd bottomPos)], div y 16 == 0]
--    positions = replicate (length messageBuffer) bottomPos
    
--    preppedOutput = zip positions surfaces
    
    

renderText :: SDL.Surface -> (Position, IO (SDL.Surface)) -> IO ()
renderText mainSurf ((x, y), textSurf) = do
  textSurf' <- textSurf
  SDL.blitSurface textSurf' Nothing mainSurf $ Just (SDL.Rect x y 0 0)
  
  freeSurface textSurf'
  return ()
  
  

createSurfaces :: Font -> [String] -> Color-> [IO SDL.Surface]
createSurfaces font texts color = do
  surfs
    where
      surfs = map (\t -> renderTextSolid font t color) texts



createPositions :: Position -> Int -> Int -> [Position]
createPositions _ 0 _ = []
createPositions p size offset = p':createPositions p' (size-1) offset
  where
    p' = (fst p, offset + snd p)


