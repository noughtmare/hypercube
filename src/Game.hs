{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Game where

import Chunk
import qualified PureChunk as P
import Types
import Faces
import Config
import Util

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
import Foreign.Storable (sizeOf)
import Linear
import Data.Foldable
import Data.IORef
import Control.Arrow
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Map as M
import Control.DeepSeq
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import System.Exit
import qualified Data.ByteString as B
import Foreign.Ptr
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

  chunkChan <- newTChanIO

  -- Generate a new game world
  game <- Game (Camera (V3 8 15 8) 0 0 5 0.001)
           <$> (realToFrac . fromJust <$> GLFW.getTime)
           <*> (fmap realToFrac . uncurry V2 <$> GLFW.getCursorPos win)
           <*> (newIORef M.empty)

  void $ flip runStateT game $ do
    -- vsync
    lift $ GLFW.swapInterval 1

    p <- lift $ do -- Shaders
      v <- GL.createShader GL.VertexShader
      vSrc <- B.readFile "hypercube.v.glsl"
      GL.shaderSourceBS v GL.$= vSrc
      GL.compileShader v
      vs <- GL.get (GL.compileStatus v)
      when (not vs) $ do
        print =<< GL.get (GL.shaderInfoLog v)
        exitFailure

      f <- GL.createShader GL.FragmentShader
      fSrc <- B.readFile "hypercube.f.glsl"
      GL.shaderSourceBS f GL.$= fSrc
      GL.compileShader f
      fs <- GL.get (GL.compileStatus f)
      when (not fs) $ do
        putStrLn =<< GL.get (GL.shaderInfoLog f)
        exitFailure

      p <- GL.createProgram
      GL.attachShader p v
      GL.attachShader p f
      GL.linkProgram p
      return p

    -- Get the shader uniforms
    [modelLoc,viewLoc,projLoc] <- lift $
      mapM (GL.get . GL.uniformLocation p) ["model","view","projection"]

    lift $ do
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

    mainLoop win chunkChan [viewLoc,projLoc,modelLoc]

keyboard :: GLFW.Window -> Float -> StateT Game IO ()
keyboard win deltaTime = do
  let handle :: MonadIO m => (GLFW.KeyState -> Bool) -> GLFW.Key -> m () -> m ()
      handle f key action = f <$> liftIO (GLFW.getKey win key) >>= \x -> when x action

      isPressed GLFW.KeyState'Pressed = True
      isPressed _ = False

  moveAmount <- (deltaTime *) <$> use (cam . speed)
  camGaze  <- use (cam . gaze)
  camSpeed <- use (cam . speed)
  camJaw   <- use (cam . jaw)

  let
    forward = moveAmount *^ rotate (axisAngle (V3 0 1 0) camJaw) (V3 0 0 (-1))
    right   = moveAmount *^ rotate (axisAngle (V3 0 1 0) camJaw) (V3 1 0 0)
    up      = moveAmount *^ V3 0 1 0

  sequence_ $ zipWith (handle isPressed)
    [GLFW.Key'W, GLFW.Key'A, GLFW.Key'S, GLFW.Key'D, GLFW.Key'Space, GLFW.Key'LeftShift]
    (map (cam . camPos +=) [forward, -right, -forward, right, up, -up])

  handle isPressed GLFW.Key'LeftControl  $ cam . speed .= 50
  handle (not . isPressed) GLFW.Key'LeftControl $ cam . speed .= 5

  handle isPressed GLFW.Key'Escape $ lift $ GLFW.setWindowShouldClose win True

mouse :: GLFW.Window -> StateT Game IO ()
mouse win = do
  currentCursorPos <- fmap realToFrac . uncurry V2 <$> lift (GLFW.getCursorPos win)
  camSensitivity <- use (cam . sensitivity)
  prevCursorPos <- use cursorPos
  let cursorDelta = camSensitivity *^ (prevCursorPos - currentCursorPos)

  cursorPos   .= currentCursorPos
  cam . jaw   += cursorDelta ^. _x
  cam . pitch += cursorDelta ^. _y

mainLoop :: GLFW.Window -> TChan (V.Vector (V4 Word8), GL.BufferObject, MVar Int) -> [GL.UniformLocation] -> StateT Game IO ()
mainLoop win chunkChan [viewLoc,projLoc,modelLoc] = do
  shouldClose <- lift $ GLFW.windowShouldClose win
  unless shouldClose $ do

    lift $ GLFW.pollEvents

    deltaTime <- do
      currentFrame <- realToFrac . fromJust <$> lift GLFW.getTime
      deltaTime <- (currentFrame -) <$> use lastFrame
      lastFrame .= currentFrame
      return deltaTime

    -- lift $ print $ 1/deltaTime

    keyboard win deltaTime
    mouse win

    -- Handle Window resize
    (width, height) <- lift $ GLFW.getFramebufferSize win
    lift $ GL.viewport GL.$=
      (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    -- Clear buffers
    lift $ GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    do -- Change view matrix
      camPos  <- use (cam . camPos)
      camGaze <- use (cam . gaze)

      viewMat <- lift $ toGLmatrix $ lookAt camPos (camPos + camGaze) (V3 0 1 0)
      GL.uniform viewLoc GL.$= viewMat

    lift $ do -- Change projection matrix
      projMat <- toGLmatrix $
        perspective (pi / 4) (fromIntegral width / fromIntegral height) 0.1 1000
      GL.uniform projLoc GL.$= projMat

    do -- Draw the chunks
      let
        loadOne = atomically (tryReadTChan chunkChan) >>= \case
          Just (vertices, vbo, var) -> do
            if (V.length vertices > 0) then do
              GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
              SV.unsafeWith (SV.convert vertices) $ \ptr ->
                GL.bufferData GL.ArrayBuffer GL.$= (fromIntegral (V.length vertices * 4),ptr,GL.StaticDraw)

              putMVar var (V.length vertices)
            else do
              putMVar var 0
          Nothing -> return ()
      lift loadOne

      w <- use world
      m <- lift $ readIORef w
      let
        render v =
          case M.lookup v m of
            Just c -> do
              c' <- P.renderChunk c modelLoc
              atomicModifyIORef w (\m' -> (M.insert v c' m', ()))
            Nothing -> do
              c <- P.newChunk v chunkChan
              --c' <- P.renderChunk c modelLoc
              atomicModifyIORef w (\m' -> (M.insert v c m', ()))

      playerPos <- fmap (\x -> floor (x / fromIntegral chunkSize)) <$> use (cam . camPos)
      let r = renderDistance
      lift $ mapM render [playerPos + V3 x y z | x <- [-r..r], y <- [-r..r], z <- [-r..r], x^2 + z^2 <= r^2]

    -- Swap the buffers (aka draw the final image to the screen)
    lift $ GLFW.swapBuffers win

    -- Recurse
    mainLoop win chunkChan [viewLoc,projLoc,modelLoc]

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
