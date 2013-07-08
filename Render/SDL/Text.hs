module Render.SDL.Text (FontAssets, loadFont, drawAll) where

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL

import Types

fontFilePath = "assets/fonts/OldStandard-Regular.ttf"

bottomPos :: Position
bottomPos = (40, 572)


namePos :: Position
namePos = (40, 100)

hpPos :: Position
hpPos = (72, 124)

hpMPos :: Position
hpMPos = (112, 124)





type FontAssets = Font


loadFont :: IO FontAssets
loadFont = do
  lol <- TTFG.init
  font <- openFont fontFilePath 14
  return font




drawAll :: World -> SDL.Surface -> Font -> IO ()
drawAll world mainSurf font = do
  drawCharacterText world mainSurf font
  drawConsoleText world mainSurf font

  




drawCharacterText :: World -> SDL.Surface -> Font -> IO ()
drawCharacterText world mainSurf font = do
  -- name:
  let nameSurf = createSurfaces font nameString (Color 20 20 20)
  renderText mainSurf (namePos, head nameSurf)
  
  -- hp:
  let hpSurf = createSurfaces font hpString (Color 200 20 20)
  renderText mainSurf (hpPos, head hpSurf)
  
  let hpMSurf = createSurfaces font hpMString (Color 200 20 20)
  renderText mainSurf (hpMPos, head hpMSurf)

  where
    hero = wHero world
    nameString = [hName hero ++ ", the " ++ (show $ hRace hero) ++ " " ++ (show $ hClass hero)]
    
    hpString = [show $ hCurrHP hero]
    hpMString = [show $ hMaxHP hero]
    


drawConsoleText :: World -> SDL.Surface -> Font -> IO ()
drawConsoleText world mainSurf font = do
  let surfaces = createSurfaces font messageBuffer (Color 40 40 40)
  let preppedOutput = zip positions surfaces
  
--  renderText mainSurf ((30, 30), renderTextSolid font (head messageBuffer) (Color 255 255 255))
  
  mapM_ (renderText mainSurf)  preppedOutput
  
  where
    messageBuffer = wMessageBuffer world
    
    positions :: [Position]
    positions = createPositions bottomPos (length messageBuffer) (-16)
    

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


