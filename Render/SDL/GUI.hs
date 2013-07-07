module Render.SDL.GUI (setup, update_, shutdown, getInput, loadAssets, Assets) where

import  Graphics.UI.SDL as SDL

import Render.SDL.Render as Render
import Render.SDL.Text as Text

import Data.IORef
import Prelude hiding(Either(..))

import Types

type Assets = (ImageAssets, FontAssets)

--problem: need to keep tabs on surfaces so they can be freed after use..


setup :: World -> Assets -> IO ()
setup world assets = do
  SDL.init [SDL.InitEverything ]
  
  -- init SDL.TTF:
--  Text.setupText
  
  setVideoMode 800 600 32 []
  setCaption "Axis of X!" "Axis of x."
  
  mainSurf <- SDL.getVideoSurface  
  
  SDL.flip mainSurf
  
    
update_ :: World -> Assets -> IO ()
update_ world ((background, tiles), font) = do
  mainSurf <- getVideoSurface
  
  -- draw BG:
  let sourceSurf = background
  let sourceRect = Just (SDL.Rect 0 0 800 600)

  let destRect = Just (SDL.Rect 0 0 0 0)
  SDL.blitSurface sourceSurf sourceRect  mainSurf destRect
  
  -- draw Tiles:
  drawWorld world mainSurf tiles
  
  -- TODO: Draw other UI elements (text etc)
  drawText world mainSurf font
  
  -- Flip!
  SDL.flip mainSurf

shutdown :: Assets -> IO ()
shutdown ((background, tiles), font) = do
  mapM_ freeSurf tiles -- tiles
  freeSurface background -- bg
  --freeSurface font
  
  SDL.quit
  print "Thanks for playing Axis of X!"
  
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
        --_ -> return (Dir Right)

        (KeyDown (Keysym key _ _)) -> do
          case key of
            SDLK_a -> return (Dir Left)
            SDLK_s -> return Wait
            SDLK_d -> return (Dir Right)
            SDLK_q -> return Exit
            _ -> getInput 
        _ -> getInput 
     

