module Render.SDL.Text (FontAssets, loadFont, drawAll) where

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL

import Types

fontFilePath = "assets/fonts/OldStandard-Regular.ttf"

consoleBottomPos :: Position
consoleBottomPos = (350, 572)

consoleBufferSize :: Int
consoleBufferSize = 18



playerNamePos :: Position
playerNamePos = (40, 100)

playerHPPos :: Position
playerHPPos = (72, 124)

playerMaxHPPos :: Position
playerMaxHPPos = (112, 124)

playerEnergyPos :: Position
playerEnergyPos = (215, 124)

playerMaxEnergyPos :: Position
playerMaxEnergyPos = (245, 124)

playerExperiencePos :: Position
playerExperiencePos = (136, 142)

playerLevelPos :: Position
playerLevelPos = (250, 142)






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
  renderText mainSurf (playerNamePos, head nameSurf)
  
  -- hp:
  let hpSurf = createSurfaces font hpString (Color 200 20 20)
  renderText mainSurf (playerHPPos, head hpSurf)
  
  let hpMSurf = createSurfaces font hpMString (Color 200 20 20)
  renderText mainSurf (playerMaxHPPos, head hpMSurf)
  
  -- energy:
  let eSurf = createSurfaces font eString (Color 20 200 20)
  renderText mainSurf (playerEnergyPos, head eSurf)
  
  let eMSurf = createSurfaces font eMString (Color 20 200 20)
  renderText mainSurf (playerMaxEnergyPos, head eMSurf)
  
  -- exp and level
  let expSurf = createSurfaces font expString (Color 20 20 20)
  renderText mainSurf (playerExperiencePos, head expSurf)
  
  let lSurf = createSurfaces font levelString (Color 20 20 20)
  renderText mainSurf (playerLevelPos, head lSurf)

  where
    hero = wHero world
    nameString = [hName hero ++ ", the " ++ (show $ hRace hero) ++ " " ++ (show $ hClass hero)]
    
    hpString = [show $ hCurrHP hero]
    hpMString = [show $ hMaxHP hero]
    
    eString = [show $ hCurrEnergy hero]
    eMString = [show $ hMaxEnergy hero]
    
    expString = [show $ hExperienceRemaining hero]
    levelString = [show $ hLevel hero]
    


drawConsoleText :: World -> SDL.Surface -> Font -> IO ()
drawConsoleText world mainSurf font = do
  let surfaces = createSurfaces font messageBuffer (Color 40 40 40)
  let preppedOutput = zip positions surfaces
  
--  renderText mainSurf ((30, 30), renderTextSolid font (head messageBuffer) (Color 255 255 255))
  
  mapM_ (renderText mainSurf)  preppedOutput
  
  where
    messageBuffer = take consoleBufferSize $ wMessageBuffer world
    
    positions :: [Position]
    positions = createPositions consoleBottomPos (length messageBuffer) (-16)
    

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


