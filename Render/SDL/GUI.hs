module Render.SDL.GUI (setup, update_, shutdown, getInput, loadAssets) where

import  Graphics.UI.SDL as SDL


import Render.SDL.Render

import Data.IORef
import Prelude hiding(Either(..))

import Types

--problem: need to keep tabs on surfaces so they can be freed after use..


setup :: World -> ImageAssets -> IO ()
setup world assets = do
  SDL.init [SDL.InitEverything ]
  setVideoMode 800 600 32 []
  setCaption "Axis of X!" "Axis of x."
  
  mainSurf <- SDL.getVideoSurface  
  
  SDL.flip mainSurf
  
    
update_ :: World -> ImageAssets -> IO ()
update_ world assets = do
  mainSurf <- getVideoSurface
  
  -- draw BG:
  let sourceSurf = fst assets
  let sourceRect = Just (SDL.Rect 0 0 800 600)

  let destRect = Just (SDL.Rect 0 0 0 0)
  SDL.blitSurface sourceSurf sourceRect  mainSurf destRect
  
  -- draw Tiles:
  drawWorld world mainSurf assets
  
  -- TODO: Draw other UI elements (text etc)
  
  
  -- Flip!
  SDL.flip mainSurf

shutdown :: ImageAssets -> IO ()
shutdown assets = do
  mapM_ freeSurf $ (snd assets) -- tiles
  freeSurface $ fst assets -- bg
  
  SDL.quit
  print "Thanks for playing Axis of X!"
  
    where
      freeSurf (_, s) = freeSurface s
  
loadAssets :: IO ImageAssets
loadAssets = loadImages

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
     

