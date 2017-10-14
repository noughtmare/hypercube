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

toGLmatrix :: M44 Float -> IO (GLmatrix Float)
toGLmatrix = newMatrix RowMajor . (toList >=> toList)

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    mapM_ GLFW.windowHint
      [   GLFW.WindowHint'Samples 4 -- 4x antialiasing
      ,   GLFW.WindowHint'ContextVersionMajor 3
      ,   GLFW.WindowHint'ContextVersionMinor 3
      ,   GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core -- OpenGL 3.3 Core Profile (or better)
      ]
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      Nothing -> return ()
      Just win -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
    GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
