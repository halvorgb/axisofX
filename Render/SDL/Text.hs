module Render.SDL.Text (FontAssets, loadFont, drawAll) where

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL

import AxisData.Common
import AxisData.World
import AxisData.Entities
import AxisData.HelpText

fontFilePath = "assets/fonts/SourceSansPro-Regular.ttf"

consoleBottomPos :: Position
consoleBottomPos = (350, 572)

consoleBufferSize :: Int
consoleBufferSize = 18

playerNamePos :: Position
playerNamePos = (40, 100)

playerHPPos :: Position
playerHPPos = (64, 127)

playerMaxHPPos :: Position
playerMaxHPPos = (104, 127)

playerEnergyPos :: Position
playerEnergyPos = (196, 127)

playerMaxEnergyPos :: Position
playerMaxEnergyPos = (226, 127)

playerExperiencePos :: Position
playerExperiencePos = (122, 148)

playerLevelPos :: Position
playerLevelPos = (218, 148)

bossNamePos :: Position
bossNamePos = (40, 244)
  
-- the help/inv/skills ++ screen
screenTopPos :: Position
screenTopPos = (40, 320)






type FontAssets = Font


loadFont :: IO FontAssets
loadFont = do
  lol <- TTFG.init
  font <- openFont fontFilePath 14
  return font




drawAll :: World -> SDL.Surface -> Font -> IO ()
drawAll world mainSurf font = do
  drawCharacterText world mainSurf font
  drawBossText world mainSurf font
  drawConsoleText world mainSurf font
  drawScreen world mainSurf font

  


drawBossText :: World -> SDL.Surface -> Font -> IO ()
drawBossText world mainSurf font = do
  let nameSurf = createSurfaces font nameString (Color 0 0 0)
  renderText mainSurf (bossNamePos, head nameSurf)
  where
    boss = wBoss world
    nameString = [(show boss)]
  


drawCharacterText :: World -> SDL.Surface -> Font -> IO ()
drawCharacterText world mainSurf font = do
  -- name:
  let nameSurf = createSurfaces font nameString (Color 0 0 0)
  renderText mainSurf (playerNamePos, head nameSurf)
  
  -- hp:
  let hpSurf = createSurfaces font hpString (Color 200 0 0)
  renderText mainSurf (playerHPPos, head hpSurf)
  
  let hpMSurf = createSurfaces font hpMString (Color 200 0 0)
  renderText mainSurf (playerMaxHPPos, head hpMSurf)
  
  -- energy:
  let eSurf = createSurfaces font eString (Color 0 200 0)
  renderText mainSurf (playerEnergyPos, head eSurf)
  
  let eMSurf = createSurfaces font eMString (Color 0 200 0)
  renderText mainSurf (playerMaxEnergyPos, head eMSurf)
  
  -- exp and level
  let expSurf = createSurfaces font expString (Color 0 0 0)
  renderText mainSurf (playerExperiencePos, head expSurf)
  
  let lSurf = createSurfaces font levelString (Color 0 0 0)
  renderText mainSurf (playerLevelPos, head lSurf)

  where
    hero = wHero world
    nameString = [(show hero)]
    
    hpString = [show $ hCurrHP hero]
    hpMString = [show $ hMaxHP hero]
    
    eString = [show $ hCurrEnergy hero]
    eMString = [show $ hMaxEnergy hero]
    
    expString = [show $ hExperienceRemaining hero]
    levelString = [show $ hLevel hero]
    

drawScreen :: World -> SDL.Surface -> Font -> IO ()
drawScreen world mainSurf font = do 
  let surfaces = createSurfaces font messageBuffer (Color 0 0 0)
  let preppedOutput = zip positions surfaces
      
  mapM_ (renderText mainSurf) preppedOutput
    where
      messageBuffer = case wScreenShown world of
        Help -> helpText
        Inv -> ["Inventory TEMP"]
        Skills -> ["Skills TEMP"]
        LevelUp -> ["LevelUp TEMP"]
      positions = createPositions screenTopPos (length messageBuffer) (16)
    


drawConsoleText :: World -> SDL.Surface -> Font -> IO ()
drawConsoleText world mainSurf font = do
  let surfaces = createSurfaces font messageBuffer (Color 40 40 40)
  let preppedOutput = zip positions surfaces
  
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
      surfs = map (\t -> renderTextBlended font t color) texts



createPositions :: Position -> Int -> Int -> [Position]
createPositions _ 0 _ = []
createPositions p size offset = p':createPositions p' (size-1) offset
  where
    p' = (fst p, offset + snd p)


