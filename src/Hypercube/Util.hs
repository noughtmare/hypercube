{-|
Module      : Hypercube.Util
Description : Utility functions
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains miscellaneous utility functions.
-}
module Hypercube.Util where

import Graphics.Rendering.OpenGL
import Data.Foldable
import Linear
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import System.Exit (exitFailure)

toGLmatrix :: M44 Float -> IO (GLmatrix Float)
toGLmatrix = newMatrix RowMajor . (toList >=> toList)

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  unless r $ putStrLn "Failed to initialize GLFW" *> GLFW.terminate *> exitFailure
  mapM_ GLFW.windowHint
      [   GLFW.WindowHint'Samples (Just 4) -- 4x antialiasing
      ,   GLFW.WindowHint'ContextVersionMajor 3
      ,   GLFW.WindowHint'ContextVersionMinor 3
      ,   GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core -- OpenGL 3.3 Core Profile (or better)
      ]
  m <- GLFW.createWindow width height title Nothing Nothing
  win <- maybe (putStrLn "Failed to open window" *> GLFW.terminate *> exitFailure) pure m
  GLFW.makeContextCurrent (Just win)
  f win
  GLFW.destroyWindow win
  GLFW.terminate
  where
  simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
  