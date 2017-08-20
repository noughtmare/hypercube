{-|
Module      : Hypercube.Input
Description : Input handling
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains all input handling code.
-}

module Hypercube.Input where

import Hypercube.Types

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad
import Linear

import qualified Graphics.UI.GLFW as GLFW

toggleCursor :: GLFW.Window -> IO ()
toggleCursor win = do
  currentMode <- GLFW.getCursorInputMode win
  GLFW.setCursorInputMode win $ case currentMode of
      GLFW.CursorInputMode'Normal -> GLFW.CursorInputMode'Disabled
      _ -> GLFW.CursorInputMode'Normal

keyCallback :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback win GLFW.Key'R _ GLFW.KeyState'Pressed _ = toggleCursor win
keyCallback _ _ _ _ _ = return ()

keyboard :: GLFW.Window -> Float -> StateT Game IO ()
keyboard win deltaTime = do
  let handle :: (GLFW.KeyState -> Bool) -> GLFW.Key -> StateT Game IO () -> StateT Game IO ()
      handle f key action = f <$> liftIO (GLFW.getKey win key) >>= \x -> when x action

      isPressed GLFW.KeyState'Pressed = True
      isPressed _ = False

  moveAmount <- (deltaTime *) <$> use (cam . speed)
  curJaw   <- use (cam . jaw)

  let
    forward = moveAmount *^ rotate (axisAngle (V3 0 1 0) curJaw) (V3 0 0 (-1))
    right   = moveAmount *^ rotate (axisAngle (V3 0 1 0) curJaw) (V3 1 0 0)
    up      = moveAmount *^ V3 0 1 0

  sequence_ $ zipWith (handle isPressed)
    [GLFW.Key'W, GLFW.Key'A, GLFW.Key'S, GLFW.Key'D, GLFW.Key'Space, GLFW.Key'LeftShift]
    (map (cam . camPos +=) [forward, -right, -forward, right, up, -up])

  handle isPressed GLFW.Key'LeftControl $ cam . speed .= 50
  handle (not . isPressed) GLFW.Key'LeftControl $ cam . speed .= 5

  handle isPressed GLFW.Key'Escape $ liftIO $ GLFW.setWindowShouldClose win True

mouse :: GLFW.Window -> Float -> StateT Game IO ()
mouse win deltaTime = do
  currentCursorPos <- fmap realToFrac . uncurry V2 <$> liftIO (GLFW.getCursorPos win)
  camSensitivity <- use (cam . sensitivity)
  prevCursorPos <- use cursorPos
  let cursorDelta = deltaTime * camSensitivity *^ (prevCursorPos - currentCursorPos)

  cursorPos   .= currentCursorPos
  cam . jaw   += cursorDelta ^. _x
  cam . pitch += cursorDelta ^. _y
