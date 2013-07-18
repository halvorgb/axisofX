module Render.SDL.GUI (setup, chooseProtagonist, update_, shutdown, getInput, loadAssets, Assets) where

import Graphics.UI.SDL as SDL
import Render.SDL.Render as Render
import Render.SDL.Text as Text

import Prelude hiding(Either(..))
import Data.Maybe
import System.Random

import Content.Names
import Content.Races
import Content.Classes

import Types.Common
import Types.Classes
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

shutdown :: World -> Assets -> IO ()
shutdown world assets@(((splashBG, background), tiles), font) = do
  
  let world' = world { wMessageBuffer = ("Press \"Q\" to quit the game."):(wMessageBuffer world) } in
    update_ world' assets
    
  let quit e = case e of
        Quit -> return ()
        (KeyDown (Keysym key _ _)) -> do
          case key of
            SDLK_q -> return ()
            _ -> (waitEventBlocking >>= quit)
        _ -> (waitEventBlocking >>= quit)
          
  input <- (waitEventBlocking >>= quit)
  
                
  
  
  
  mapM_ freeSurf tiles -- tiles
  freeSurface background -- bg
  freeSurface splashBG
  --freeSurface font
  
  SDL.quit
  
  where
      freeSurf (_, s) = freeSurface s
  
loadAssets :: IO Assets
loadAssets = do
  imageAssets <- loadImages
  fontAsset   <- loadFont
  return (imageAssets, fontAsset)

getInput ::  IO Input
getInput  = do
  waitEventBlocking >>= handleInput
    where
      handleInput e = case e of
        Quit -> return Exit
        (KeyDown (Keysym key _ _)) -> do
          case key of
            SDLK_z -> return (Show Skills)
            SDLK_x -> return (Show Inv)
            SDLK_c -> return (Show Help)
            SDLK_v -> return (Show LevelUp)
            
            SDLK_a -> return (Dir Left)
            SDLK_s -> return Wait
            SDLK_d -> return (Dir Right)
            SDLK_q -> return Exit
            _ -> getInput 
        _ -> getInput 
     





----------------
        
        ---
        ---
        -- selecing name/class/race
        



-- bit ugly for this to be here...
chooseProtagonist :: Assets -> IO (String, Class, Race)
chooseProtagonist (((splashBG, _), _), font) = do
  
  mainSurf <- getVideoSurface
  
  -- draw BG:
  let sourceRect = Just (SDL.Rect 0 0 800 600)

  let destRect = Just (SDL.Rect 0 0 0 0)
  SDL.blitSurface splashBG sourceRect mainSurf destRect
  
  -- name = random!
  nameGen <- getStdGen
  let name = randomName nameGen
  let welcomePos = (256, 296)
  
  drawTextAtPos ("Welcome to Axis of X,  your name is " ++ name ++ ".") welcomePos mainSurf font
  
  
  let klassTextPos = (256, 328)
  drawTextAtPos "Select a class:" klassTextPos mainSurf font
  SDL.flip mainSurf
  let klassOptionsPos = (256, 360)
  klass <- chooseFromList classes klassOptionsPos mainSurf font
  let klassPos = (256, 396)
  drawTextAtPos ("> " ++ (show klass)) klassPos mainSurf font
  
  
  let raceTextPos = (256, 428)
  drawTextAtPos "Select a race:" raceTextPos mainSurf font
  let raceOptionsPos = (256, 460)
  SDL.flip mainSurf
  race <- chooseFromList races raceOptionsPos mainSurf font
  
  return (name, klass, race)




-- Used to pick classes and races.
chooseFromList :: Show a => Eq a => [a] -> Position -> SDL.Surface -> FontAssets -> IO a
chooseFromList list pos mainSurf font = do  
  drawTextAtPos outStr pos mainSurf font
  SDL.flip mainSurf
  choice <- getChoice lastChoice
  return $ fromJust $ lookup choice charToList
  
    where
      charToList = zip ['a'..'z'] list
      charToListString = map (\(c, cls) -> (c, show cls)) charToList
      outStr = foldl (\str (c, s) -> str ++ (c:": " ++ s ++ "  " )) "" charToListString
      lastChoice = fst $ last charToList
      
-- Used  to get a char bounded [a..maxChar].
getChoice :: Char -> IO Char
getChoice maxChar = do
  waitEventBlocking >>= getCharInput
    where
      getCharInput e = case e of
        (KeyDown (Keysym  _ _ c)) -> do 
          if c <= maxChar
            then
            return c
            else
            waitEventBlocking >>= getCharInput
            
        _ -> waitEventBlocking >>= getCharInput
             