module Render.SDL.Text (FontAssets, loadFont, drawGameText, drawTextAtPos) where

import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL

import Data.List

import Helpers
import Types.Common
import Types.World
import Content.StaticText

fontFilePath = "assets/fonts/SourceSansPro-Regular.ttf"

consoleBottomPos :: Position
consoleBottomPos = (350, 572)

consoleTopPos :: Position
consoleTopPos = (350, 256)

consoleBufferSize :: Int
consoleBufferSize = 18

---
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
----

---
skillQueueOnePos :: Position
skillQueueOnePos = (362, 182)

skillQueueTwoPos :: Position
skillQueueTwoPos = (476, 182)

skillQueueThreePos :: Position
skillQueueThreePos = (588, 182)

skillQueueFourPos :: Position
skillQueueFourPos = (700, 182)

skillQueueEnergyPos :: Position
skillQueueEnergyPos = (434, 232)

skillQueueSpeedPos :: Position
skillQueueSpeedPos = (557, 232)

--
statusTimePos :: Position
statusTimePos = (480, 103)




bossNamePos :: Position
bossNamePos = (48, 258)



  
vicinityBufferSize :: Int
vicinityBufferSize = 15

vicinityBotPos :: Position
vicinityBotPos = (40, 572)

                 




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
  drawVicinity world mainSurf font
  
  drawWorldStatus world mainSurf font
  drawSkillQueue world mainSurf font
  drawScreen world mainSurf font

  

drawWorldStatus :: World -> SDL.Surface -> Font -> IO ()
drawWorldStatus world mainSurf font =
  renderText mainSurf (statusTimePos, head timeSurf)
    where
      timeString = [show $ wTimeElapsed world]
      timeSurf = createSurfaces font timeString (Color 0 0 0)
      

drawBossText :: World -> SDL.Surface -> Font -> IO ()
drawBossText world mainSurf font = do
  renderText mainSurf (bossNamePos, head nameSurf)
  where
    boss = wBoss world
    nameString = [show boss]
    
    nameSurf = createSurfaces font nameString (Color 0 0 0)
  
drawSkillQueue :: World -> SDL.Surface -> Font -> IO ()
drawSkillQueue world mainSurf font = do
  renderText mainSurf (skillQueueOnePos, head oneSurf)
  renderText mainSurf (skillQueueTwoPos, head twoSurf)  
  renderText mainSurf (skillQueueThreePos, head threeSurf)  
  renderText mainSurf (skillQueueFourPos, head fourSurf)
  
  renderText mainSurf (skillQueueEnergyPos, head energySurf)
  renderText mainSurf (skillQueueSpeedPos, head speedSurf)
  
  where
    hero = wHero world
    
    oneString = [show $ first $ hSkillQueue hero]
    twoString = [show $ second $ hSkillQueue hero]    
    threeString = [show $ third $ hSkillQueue hero]    
    fourString = [show $ fourth $ hSkillQueue hero]    
    oneSurf = createSurfaces font oneString (Color 0 0 0)
    twoSurf = createSurfaces font twoString (Color 0 0 0)
    threeSurf = createSurfaces font threeString (Color 0 0 0)
    fourSurf = createSurfaces font fourString (Color 0 0 0)
    
    
    energyCost = foldl' (+) 0 $ map (\(i,s) -> skillEnergyCost s hero i) $ zip [1..] $ toList $ hSkillQueue hero
    energyString = [show energyCost]
    energySurf = createSurfaces font energyString (Color 0 0 0)
    
    speedCost = foldl' (+) 0 $ map (\s -> skillSpeedCost s hero) $ toList $ hSkillQueue hero
    speedString = [show speedCost]
    speedSurf = createSurfaces font speedString (Color 0 0 0)
  

drawCharacterText :: World -> SDL.Surface -> Font -> IO ()
drawCharacterText world mainSurf font = do
  -- name:
  renderText mainSurf (playerNamePos, head nameSurf)
  
  -- hp:
  renderText mainSurf (playerHPPos, head hpSurf)
  
  renderText mainSurf (playerMaxHPPos, head hpMSurf)
  
  -- energy:
  renderText mainSurf (playerEnergyPos, head eSurf)

  renderText mainSurf (playerMaxEnergyPos, head eMSurf)
  
  -- exp and level
  renderText mainSurf (playerExperiencePos, head expSurf)
  
  renderText mainSurf (playerLevelPos, head lSurf)
  
  
  -- speed
  renderText mainSurf (playerSpeedPos, head spdSurf)
  
  -- dies.
  renderText mainSurf (playerHitDiePos, head hitDieSurf)
  
  renderText mainSurf (playerDmgDiePos, head dmgDieSurf)
  
  renderText mainSurf (playerEvadeDiePos, head evdDieSurf)  
  
  -- mitigation
  renderText mainSurf (playerMitigationPos, head mitSurf)
  
  -- reputation:
  renderText mainSurf (playerRepPos, head repSurf)

  where
    hero = wHero world
    nameString = [showLong hero]
    nameSurf = createSurfaces font nameString (Color 0 0 0)
    
    hpString = [show $ eCurrHP hero]
    hpSurf = createSurfaces font hpString (Color 200 0 0)
    hpMString = [show $ eMaxHP hero]
    hpMSurf = createSurfaces font hpMString (Color 200 0 0)
    
    eString = [show $ hCurrEnergy hero]
    eSurf = createSurfaces font eString (Color 0 200 0)
    eMString = [show $ hMaxEnergy hero]
    eMSurf = createSurfaces font eMString (Color 0 200 0)
    
    expString = [show $ hExperienceRemaining hero]
    expSurf = createSurfaces font expString (Color 0 0 0)
    levelString = [show $ hLevel hero]
    lSurf = createSurfaces font levelString (Color 0 0 0)
    
    speedString = [show $ eSpeed hero]
    spdSurf = createSurfaces font speedString (Color 0 0 0)
    
    hitDieString = [show $ eHitDie hero]
    hitDieSurf = createSurfaces font hitDieString (Color 0 0 0)
    dmgDieString = [show $ eDamageDie hero]
    dmgDieSurf = createSurfaces font dmgDieString (Color 0 0 0)
    evdDieString = [show $ eEvadeDie hero]
    evdDieSurf = createSurfaces font evdDieString (Color 0 0 0)
    
    mitString = [show $ eMitigation hero]
    mitSurf = createSurfaces font mitString (Color 0 0 0)
    repString = [show $ hReputation hero]
    repSurf = createSurfaces font repString (Color 0 0 0)
    

drawVicinity :: World -> SDL.Surface -> Font -> IO ()
drawVicinity world mainSurf font = do
  mapM_ (renderText mainSurf) preppedOutput
    where -- tweak dat --V
      visibleEnts = take vicinityBufferSize $ getEntitiesFromViewFrame world $ getViewFrame world
--      h = wHero world
      entString = map showLong visibleEnts
      entSurf = createSurfaces font entString (Color 0 0 0)
      
      positions = createPositions vicinityBotPos (length visibleEnts) (-16)
      preppedOutput = zip positions entSurf
      
      



drawScreen :: World -> SDL.Surface -> Font -> IO ()
drawScreen world mainSurf font = do 
  let surfaces = createSurfaces font messageBuffer (Color 0 0 0)
  let preppedOutput = zip positions surfaces
      
  mapM_ (renderText mainSurf) preppedOutput
    where
      heroSkills = hSkills $ wHero world
      chars = ['a'..'z']
      c2s = zip chars heroSkills
      
      messageBuffer = case wScreenShown world of
        Help -> helpText
        Inv -> ["Inventory TEMP"]
        Skills -> skillsText ++ map (\(c,s) -> c:": " ++ show s) c2s
        LevelUp -> ["LevelUp TEMP"]
        Console -> take consoleBufferSize $ wMessageBuffer world
      positions = 
        case wScreenShown world of 
          Console -> createPositions consoleBottomPos (length messageBuffer) (-16)
          _       -> createPositions consoleTopPos (length messageBuffer) 16

drawConsoleText :: World -> SDL.Surface -> Font -> IO ()
drawConsoleText world mainSurf font = do
  let surfaces = createSurfaces font messageBuffer (Color 40 40 40)
  let preppedOutput = zip positions surfaces
  
  mapM_ (renderText mainSurf)  preppedOutput
  
  where
    messageBuffer = take consoleBufferSize $ wMessageBuffer world
    
    positions :: [Position]
    positions = createPositions consoleBottomPos (length messageBuffer) (-16)
    

renderText :: SDL.Surface -> (Position, IO SDL.Surface) -> IO ()
renderText mainSurf ((x, y), textSurf) = do
  textSurf' <- textSurf
  SDL.blitSurface textSurf' Nothing mainSurf $ Just (SDL.Rect x y 0 0)
  
  freeSurface textSurf'
  return ()
  
  

createSurfaces :: Font -> [String] -> Color -> [IO SDL.Surface]
createSurfaces font texts color = map (\t -> renderTextBlended font t color) texts




createPositions :: Position -> Int -> Int -> [Position]
createPositions _ 0 _ = []
createPositions p size offset = p':createPositions p' (size-1) offset
  where
    p' = (fst p, offset + snd p)


