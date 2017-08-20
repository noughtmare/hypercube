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
