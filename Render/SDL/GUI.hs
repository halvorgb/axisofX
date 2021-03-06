module Render.SDL.GUI (setup, chooseProtagonist, chooseSkill, update_, shutdown, delayedShutdown, getInput, loadAssets, Assets) where

import Graphics.UI.SDL as SDL
import Render.SDL.Render as Render
import Render.SDL.Text as Text

import Prelude hiding(Either(..))
import Data.Maybe
import System.Random
import System.Exit
import qualified Data.Char as C
import Data.List

import Content.Names
import Content.Races
import Content.Classes

import Types.Common
import Types.World

type Assets = (ImageAssets, FontAssets)

setup :: IO ()
setup  = do
  SDL.init [ SDL.InitEverything ]
  SDL.enableUnicode True
    
  setVideoMode 800 600 32 []
  setCaption "Axis of X!" "Axis of X."
  
  mainSurf <- SDL.getVideoSurface  
  
  SDL.flip mainSurf
  
  
  
    
update_ :: World -> Assets -> IO ()
update_ world (((_, background), tiles), font) = do
  mainSurf <- getVideoSurface
  
  -- draw BG:
  let sourceSurf = background
  let sourceRect = Just (SDL.Rect 0 0 800 600)

  let destRect = Just (SDL.Rect 0 0 0 0)
  SDL.blitSurface sourceSurf sourceRect  mainSurf destRect
  
  -- draw Tiles:
  drawWorld world mainSurf tiles
  
  -- Draw Text!
  drawGameText world mainSurf font
  
  -- Flip!
  SDL.flip mainSurf

-- called if forced end to the game, Ex: player dies.
delayedShutdown :: World -> Assets -> IO ()
delayedShutdown world assets = do
  let world' = world { wMessageBuffer = "Press \"Q\", \"<M>-F4\" or the X to quit the game.":wMessageBuffer world } in
    update_ world' assets
  let quit e = case e of
        Quit -> return ()
        (KeyDown (Keysym key _ _)) ->
          case key of
            SDLK_q -> return ()
            _ -> waitEventBlocking >>= quit
        _ -> waitEventBlocking >>= quit
          
  input <- waitEventBlocking >>= quit
  
  shutdown world assets
  
-- Called when explicitly asked to close the game.
shutdown :: World -> Assets -> IO ()
shutdown world assets@(((splashBG, background), tiles), font) = do  
  mapM_ freeSurf tiles -- tiles
  freeSurface background -- bg
  freeSurface splashBG -- bg
  
  SDL.quit  
    where
      freeSurf (_, s) = freeSurface s
  
loadAssets :: IO Assets
loadAssets = do
  imageAssets <- loadImages
  fontAsset   <- loadFont
  return (imageAssets, fontAsset)

getInput ::  IO Input
getInput  =
  waitEventBlocking >>= handleInput
    where
      handleInput e = case e of
        Quit -> return Exit
        (KeyDown (Keysym key _ char)) ->
          case (key, char) of
            (_, '@') -> return (Show Inv)
            (_, '?') -> return (Show Help)            
            (SDLK_z, _) -> return (Show Skills)
            (SDLK_x, _) -> return (Show Inv)
            (SDLK_c, _) -> return (Show Help)
            (SDLK_v, _) -> return (Show LevelUp)
            
            (SDLK_a, _) -> return (Dir Left)
            (SDLK_s, _) -> return Wait
            (SDLK_d, _) -> return (Dir Right)
            (SDLK_q, _) -> return Exit
            (SDLK_r, _) -> return Rest
            
            (SDLK_1, _) -> return (Queue 1)
            (SDLK_2, _) -> return (Queue 2)
            (SDLK_3, _) -> return (Queue 3)
            (SDLK_4, _) -> return (Queue 4)
            (SDLK_RETURN, _) -> return ExecuteSkills
            (SDLK_ESCAPE, _) -> return (Show Console)
            _ -> getInput 
        _ -> getInput 
     

-- Called after the player brings up the queue screen.
chooseSkill :: World -> Assets -> IO Skill
chooseSkill world assets = do
  -- Main already set Skills as visible. call update!
  update_ world assets
  choice <- getChoice lastChoice world assets
  
  if null heroSkills || isNothing choice
    then
    return NoSkill
    else
    return $ fromMaybe NoSkill $ lookup (fromJust choice) c2s  
    where
      heroSkills = hSkills $ wHero world
      c2s = zip ['a'..'z'] heroSkills
      
      lastChoice = fst $ last c2s
  
  

----------------
        
        ---
        ---
        -- selecing name/class/race
        
-- bit ugly for this to be here...?
chooseProtagonist :: World -> Assets -> IO (String, Class, Race)
chooseProtagonist w a@(((splashBG, _), _), font) = do
  
  mainSurf <- getVideoSurface
  
  -- draw BG:
  let sourceRect = Just (SDL.Rect 0 0 800 600)
  let destRect = Just (SDL.Rect 0 0 0 0)
  SDL.blitSurface splashBG sourceRect mainSurf destRect
  
  -- name = random!
  nameGen <- getStdGen
  let name = randomHeroName nameGen
  let welcomePos = (256, 296)
  drawTextAtPos ("Welcome to Axis of X,  your name is " ++ name ++ ".") welcomePos mainSurf font
  
  let klassTextPos = (256, 328)
  drawTextAtPos "Select a class:" klassTextPos mainSurf font
  SDL.flip mainSurf
  let klassOptionsPos = (256, 360)
  klass <- chooseFromList classes klassOptionsPos mainSurf font w a
  let klassPos = (256, 396)
  drawTextAtPos ("> " ++ show klass) klassPos mainSurf font
  
  
  let raceTextPos = (256, 428)
  drawTextAtPos "Select a race:" raceTextPos mainSurf font
  let raceOptionsPos = (256, 460)
  SDL.flip mainSurf
  race <- chooseFromList races raceOptionsPos mainSurf font w a
  
  return (name, klass, race)


-- Used to pick classes and races.
chooseFromList :: Show a => Eq a => [a] -> Position -> SDL.Surface -> FontAssets -> World -> Assets -> IO a
chooseFromList list pos mainSurf font w a = do  
  drawTextAtPos outStr pos mainSurf font
  SDL.flip mainSurf
  
  choice <- getChoice lastChoice w a
  if isNothing choice
    then
    chooseFromList list pos mainSurf font w a
    else
    return $ fromJust $ lookup (fromJust choice) charToList  
    where
      charToList = zip ['a'..'z'] list
      charToListString = map (\(c, cls) -> (c, show cls)) charToList
      outStr = foldl' (\str (c, s) -> str ++ (c:": " ++ s ++ "  " )) "" charToListString
      lastChoice = fst $ last charToList
      
-- Used  to get a char bounded [a..maxChar]. (world and assets are sent to handle quits)
getChoice :: Char -> World -> Assets -> IO (Maybe Char)
getChoice maxChar w a =
  waitEventBlocking >>= getCharInput
    where
      getCharInput e = case e of
        Quit -> do
          shutdown w a
          exitWith ExitSuccess
        (KeyDown (Keysym  k _ c)) ->
          case k of
            SDLK_ESCAPE -> return Nothing
            _ -> if c <= maxChar && C.isAsciiLower c -- exclude meta keys etc.
                 then
                   return (Just c)
                 else do
                   waitEventBlocking >>= getCharInput
            
        _ -> waitEventBlocking >>= getCharInput
             