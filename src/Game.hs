{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Chunk
import Types
import Config
import Util
import Shaders

-- import qualified Debug.Trace as D

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
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

switchCursor :: GLFW.Window -> IO ()
switchCursor win = do
  currentMode <- GLFW.getCursorInputMode win
  GLFW.setCursorInputMode win $ case currentMode of
      GLFW.CursorInputMode'Normal -> GLFW.CursorInputMode'Disabled
      _ -> GLFW.CursorInputMode'Normal

keyCallback :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback win GLFW.Key'R _ GLFW.KeyState'Pressed _ = switchCursor win
keyCallback _ _ _ _ _ = return ()

start :: IO ()
start = withWindow 1280 720 "Jaro's minecraft ripoff [WIP]" $ \win -> do
  -- Disable the cursor
  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

  chan <- newTChanIO

  -- Generate a new game world
  game <- Game (Camera (V3 8 15 8) 0 0 5 0.001)
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
        (Right texture) <- U.readTexture "stone.png"
        GL.textureBinding GL.Texture2D GL.$= Just texture
        GL.textureFilter  GL.Texture2D GL.$= ((GL.Nearest,Nothing),GL.Nearest)

        GL.generateMipmap' GL.Texture2D

      -- Set clear color
      GL.clearColor GL.$= GL.Color4 0.2 0.3 0.3 1

      GLFW.setKeyCallback win (Just keyCallback)

      GL.cullFace  GL.$= Just GL.Back
      GL.depthFunc GL.$= Just GL.Less

    mainLoop win chan viewLoc projLoc modelLoc

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

mouse :: GLFW.Window -> StateT Game IO ()
mouse win = do
  currentCursorPos <- fmap realToFrac . uncurry V2 <$> liftIO (GLFW.getCursorPos win)
  camSensitivity <- use (cam . sensitivity)
  prevCursorPos <- use cursorPos
  let cursorDelta = camSensitivity *^ (prevCursorPos - currentCursorPos)

  cursorPos   .= currentCursorPos
  cam . jaw   += cursorDelta ^. _x
  cam . pitch += cursorDelta ^. _y

draw :: GL.UniformLocation -> TChan (V.Vector (V4 Word8), GL.BufferObject, MVar Int) -> StateT Game IO ()
draw modelLoc chan = do
  m <- use world
  let
    render v =
      case M.lookup v m of
        Just c -> do
          c' <- liftIO $ renderChunk c modelLoc
          world %= M.insert v c'
        Nothing -> do
          c <- liftIO $ newChunk v chan
          c' <- liftIO $ renderChunk c modelLoc
          world %= M.insert v c'

  playerPos <- fmap (\x -> floor (x / fromIntegral chunkSize)) <$> use (cam . camPos)
  let r = renderDistance
  mapM_ render [playerPos + V3 x y z | x <- [-r..r], y <- [-r..r], z <- [-r..r], x^(2::Int) + z^(2::Int) <= r^(2::Int)]

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
    mouse win

    -- Handle Window resize
    (width, height) <- liftIO $ GLFW.getFramebufferSize win
    liftIO $ GL.viewport GL.$=
      (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    -- Clear buffers
    liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    do -- Change view matrix
      curCamPos  <- use (cam . camPos)
      curGaze <- use (cam . gaze)

      viewMat <- liftIO $ toGLmatrix $ lookAt curCamPos (curCamPos + curGaze) (V3 0 1 0)
      GL.uniform viewLoc GL.$= viewMat

    liftIO $ do -- Change projection matrix
      projMat <- toGLmatrix $
        perspective (pi / 4) (fromIntegral width / fromIntegral height) 0.1 1000
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

    draw modelLoc chan

    liftIO $ GLFW.swapBuffers win

    mainLoop win chan viewLoc projLoc modelLoc

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
