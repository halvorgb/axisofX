module Render.SDL.Text (FontAssets, loadFont, drawGameText, drawTextAtPos) where

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL

import Types.Common
import Types.World
import Types.Classes
import Content.StaticText

fontFilePath = "assets/fonts/SourceSansPro-Regular.ttf"

consoleBottomPos :: Position
consoleBottomPos = (350, 572)

consoleBufferSize :: Int
consoleBufferSize = 18

playerNamePos :: Position
playerNamePos = (48, 82)

playerHPPos :: Position
playerHPPos = (64, 102)

playerMaxHPPos :: Position
playerMaxHPPos = (104, 102)

playerEnergyPos :: Position
playerEnergyPos = (196, 102)

playerMaxEnergyPos :: Position
playerMaxEnergyPos = (226, 102)

playerExperiencePos :: Position
playerExperiencePos = (70, 123)

playerLevelPos :: Position
playerLevelPos = (150, 123)

playerSpeedPos :: Position
playerSpeedPos = (216, 123)

playerHitDiePos :: Position
playerHitDiePos = (64, 144)

playerDmgDiePos :: Position
playerDmgDiePos = (188, 144)

playerEvadeDiePos :: Position
playerEvadeDiePos = (85, 165)

playerMitigationPos :: Position
playerMitigationPos = (234, 165)

playerRepPos :: Position
playerRepPos = (122, 186)




bossNamePos :: Position
bossNamePos = (48, 226)
  
-- the help/inv/skills ++ screen
screenTopPos :: Position
screenTopPos = (40, 320)






type FontAssets = Font


loadFont :: IO FontAssets
loadFont = do
  lol <- TTFG.init
  font <- openFont fontFilePath 14
  return font



drawTextAtPos :: String -> Position -> SDL.Surface -> Font -> IO ()
drawTextAtPos string pos mainSurf font = do
  let surf = createSurfaces font [string] (Color 255 255 255)
  renderText mainSurf (pos, head surf)





drawGameText :: World -> SDL.Surface -> Font -> IO ()
drawGameText world mainSurf font = do
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
  
  
  -- speed
  let spdSurf = createSurfaces font speedString (Color 0 0 0)
  renderText mainSurf (playerSpeedPos, head spdSurf)
  
  -- dies.
  let hitDieSurf = createSurfaces font hitDieString (Color 0 0 0)
  renderText mainSurf (playerHitDiePos, head hitDieSurf)
  
  let dmgDieSurf = createSurfaces font dmgDieString (Color 0 0 0)
  renderText mainSurf (playerDmgDiePos, head dmgDieSurf)
  
  let evdDieSurf = createSurfaces font evdDieString (Color 0 0 0)
  renderText mainSurf (playerEvadeDiePos, head evdDieSurf)  
  
  -- mitigation
  let mitSurf = createSurfaces font mitString (Color 0 0 0)
  renderText mainSurf (playerMitigationPos, head mitSurf)
  
  -- reputation:
  let repSurf = createSurfaces font repString (Color 0 0 0)
  renderText mainSurf (playerRepPos, head repSurf)

  where
    hero = wHero world
    nameString = [(showLong hero)]
    
    hpString = [show $ eCurrHP hero]
    hpMString = [show $ eMaxHP hero]
    
    eString = [show $ hCurrEnergy hero]
    eMString = [show $ hMaxEnergy hero]
    
    expString = [show $ hExperienceRemaining hero]
    levelString = [show $ hLevel hero]
    
    speedString = [show $ eSpeed hero]
    
    hitDieString = [show $ eHitDie hero]
    dmgDieString = [show $ eDamageDie hero]
    evdDieString = [show $ eEvadeDie hero]
    
    mitString = [show $ eMitigation hero]
    
    repString = ["Asshole(temp)"]
    

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


