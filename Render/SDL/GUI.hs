module Render.SDL.GUI (setup, update, shutdown, getInput) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL as SDLi

import Render.SDL.Render

import Data.IORef
import Prelude hiding(Either(..))

import Types

--problem: need to keep tabs on surfaces so they can be freed after use..


setup world = do
  SDL.init [SDL.InitEverything ]
  SDL.setVideoMode 1280 600 32 []
  SDL.setCaption "Axis of X!" "Axis of x."
  
  mainSurf <- SDL.getVideoSurface  
  
  SDL.flip mainSurf
  
update world = do
  mainSurf <- SDL.getVideoSurface
  drawWorld world mainSurf
  SDL.flip mainSurf

shutdown = do
  SDL.quit
  print "Thanks for playing Axis of X!"
  
getInput = do
  SDL.waitEventBlocking >>= handleInput
    where
      handleInput e = case e of
        SDL.Quit -> return Exit
        _ -> return (Dir Right)