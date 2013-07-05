module Render.SDL.GUI (setup, update, shutdown, getInput, loadAssets) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL as SDLi

import Render.SDL.Render as R

import Data.IORef
import Prelude hiding(Either(..))

import Types

--problem: need to keep tabs on surfaces so they can be freed after use..


setup world = do
  SDL.init [SDL.InitEverything ]
  SDL.setVideoMode 800 600 32 []
  SDL.setCaption "Axis of X!" "Axis of x."
  
  mainSurf <- SDL.getVideoSurface  
  
  SDL.flip mainSurf
  
    
update world assets = do
  mainSurf <- SDL.getVideoSurface
--  asssets <- loadAssets
  drawWorld world mainSurf assets
  SDL.flip mainSurf
  --return ()

shutdown = do
  SDL.quit
  print "Thanks for playing Axis of X!"
  
  
loadAssets = loadImages
  
getInput = do
  SDL.waitEventBlocking >>= handleInput
    where
      handleInput e = case e of
        SDL.Quit -> return Exit
        _ -> return Wait
--        _ -> return (Dir Right)