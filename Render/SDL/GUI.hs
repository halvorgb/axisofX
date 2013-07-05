module Render.SDL.GUI (setup, update_, shutdown, getInput, loadAssets) where

import  Graphics.UI.SDL as SDL


import Render.SDL.Render

import Data.IORef
import Prelude hiding(Either(..))

import Types

--problem: need to keep tabs on surfaces so they can be freed after use..


setup world assets = do
  SDL.init [SDL.InitEverything ]
  setVideoMode 800 600 32 []
  setCaption "Axis of X!" "Axis of x."
  
  mainSurf <- SDL.getVideoSurface  
  
  SDL.flip mainSurf
  
    

update_ world assets = do
  mainSurf <- getVideoSurface
  drawWorld world mainSurf assets
  SDL.flip mainSurf

shutdown  = do
--  mapM_ freeSurf assets
  
  SDL.quit
  print "Thanks for playing Axis of X!"
  
--    where
--      freeSurf (_, s) = freeSurface s
  
  
loadAssets = loadImages
  
getInput = do
  waitEventBlocking >>= handleInput
    where
      handleInput e = case e of
        Quit -> return Exit
        (KeyDown (Keysym key _ _)) -> do
          case key of
            SDLK_a -> return (Dir Left)
            SDLK_s -> return Wait
            SDLK_d -> return (Dir Right)
            SDLK_q -> return Exit
            _ -> getInput
        _ -> getInput
     

       
  
--        _ -> return (Dir Right)