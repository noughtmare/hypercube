{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Hypercube.Game
Description : The main game loops and start function
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains the @start@ function, the @mainLoop@ and the @draw@ function.

TODO: find a better place for the @draw@ function.
-}

module Hypercube.Game where

import Hypercube.Chunk
import Hypercube.Types
import Hypercube.Config
import Hypercube.Util
import Hypercube.Shaders
import Hypercube.Input

-- import qualified Debug.Trace as D

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Lens
import Control.Concurrent
import Data.Maybe
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as U
import Graphics.UI.GLFW as GLFW
import Linear
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Map as M
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import System.Exit
import qualified Data.ByteString as B
import Data.Word (Word8)


start :: IO ()
start = withWindow 1280 720 "Hypercube" $ \win -> do
  -- Disable the cursor
  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

  chan <- newTChanIO

  -- Generate a new game world
  game <- Game (Camera (V3 8 15 8) 0 0 5 0.05)
           <$> (realToFrac . fromJust <$> GLFW.getTime)
           <*> (fmap realToFrac . uncurry V2 <$> GLFW.getCursorPos win)
           <*> (pure M.empty)

  void $ flip runStateT game $ do
    -- vsync
    liftIO $ GLFW.swapInterval 1

    p <- liftIO shaders

    -- Get the shader uniforms
    [modelLoc,viewLoc,projLoc] <- liftIO $
      mapM (GL.get . GL.uniformLocation p) ["model","view","projection"]

    liftIO $ do
      -- Set shader
      GL.currentProgram GL.$= Just p

      do -- Load texture & generate mipmaps
        (Right texture) <- U.readTexture "blocks.png"
        GL.textureBinding GL.Texture2D GL.$= Just texture
        GL.textureFilter  GL.Texture2D GL.$= ((GL.Nearest,Nothing),GL.Nearest)

        GL.generateMipmap' GL.Texture2D

      -- Set clear color
      GL.clearColor GL.$= GL.Color4 0.2 0.3 0.3 1

      GLFW.setKeyCallback win (Just keyCallback)

      GL.cullFace  GL.$= Just GL.Back
      GL.depthFunc GL.$= Just GL.Less

    mainLoop win chan viewLoc projLoc modelLoc

draw :: M44 Float -> GL.UniformLocation -> TChan (V.Vector (V4 Word8), GL.BufferObject, MVar Int) -> StateT Game IO ()
draw vp modelLoc chan = do
  m <- use world
  let
    render v =
      case M.lookup v m of
        Just c -> do
          c' <- liftIO $ execStateT (renderChunk modelLoc) c
          world %= M.insert v c'
        Nothing -> do
          c <- liftIO $ newChunk v chan
          c' <- liftIO $ execStateT (renderChunk modelLoc) c
          world %= M.insert v c'

  playerPos <- fmap (\x -> floor (x / fromIntegral chunkSize)) <$> use (cam . camPos)
  let r = renderDistance
  mapM_ render $ do
    v <- V3 <$> [-r..r] <*> [-r..r] <*> [-r..r]
    -- Filter out chunks that are ouside the screen (WIP)
    let toRender = playerPos + v
        projected = (\x -> x ^/ (x ^. _w))
          $ vp !*! (identity & translation .~ (fromIntegral <$> (chunkSize *^ toRender))) 
               !*  (1 & _xyz .~ fromIntegral chunkSize / 2)
        diameter = fromIntegral chunkSize * sqrt 3
    guard $ projected ^. _z >= -diameter
    guard $ all ((<= 1 + diameter / abs (projected ^. _w)) . abs) $ (\x -> x ^. _xy) projected
    return $ toRender

mainLoop :: GLFW.Window -> TChan (V.Vector (V4 Word8), GL.BufferObject, MVar Int) -> GL.UniformLocation -> GL.UniformLocation -> GL.UniformLocation -> StateT Game IO ()
mainLoop win chan viewLoc projLoc modelLoc = do
  shouldClose <- liftIO $ GLFW.windowShouldClose win
  unless shouldClose $ do

    liftIO $ GLFW.pollEvents

    deltaTime <- do
      currentFrame <- realToFrac . fromJust <$> liftIO GLFW.getTime
      deltaTime <- (currentFrame -) <$> use lastFrame
      lastFrame .= currentFrame
      return deltaTime

    -- liftIO $ print $ 1/deltaTime

    keyboard win deltaTime
    mouse win deltaTime

    -- Handle Window resize
    (width, height) <- liftIO $ GLFW.getFramebufferSize win
    liftIO $ GL.viewport GL.$=
      (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    -- Clear buffers
    liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- Change view matrix
    curCamPos  <- use (cam . camPos)
    curGaze <- use (cam . gaze)

    let view = lookAt curCamPos (curCamPos + curGaze) (V3 0 1 0)
    viewMat <- liftIO $ toGLmatrix view
    GL.uniform viewLoc GL.$= viewMat

    -- Change projection matrix
    let proj = perspective (pi / 4) (fromIntegral width / fromIntegral height) 0.1 1000
    projMat <- liftIO $ toGLmatrix proj
    GL.uniform projLoc GL.$= projMat

    do
      deadLine <- liftIO $ (+ 0.001) . fromJust <$> GLFW.getTime
      let
        loadVbos = do
          atomically (tryReadTChan chan) >>= \case
            Just (vertices, vbo, var) -> do
              if (V.length vertices > 0) then do
                GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
                U.replaceVector GL.ArrayBuffer (SV.convert vertices)
                putMVar var (V.length vertices)
              else do
                putMVar var 0
            Nothing -> return ()
          now <- fromJust <$> GLFW.getTime
          when (now < deadLine) $ loadVbos
      liftIO $ loadVbos

    draw (proj !*! view) modelLoc chan

    liftIO $ GLFW.swapBuffers win

    mainLoop win chan viewLoc projLoc modelLoc

