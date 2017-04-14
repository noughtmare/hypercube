{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE BangPatterns #-} 

module Lib (gameLoop) where

import Types
import Faces
import Config

-- import qualified Debug.Trace as D

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Lens
import Control.Concurrent
import Data.Maybe
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.GLUtil as U
import Foreign.Storable (sizeOf)
import Linear
import Data.Foldable
import Data.IORef
import Control.Arrow
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.DeepSeq
import Control.Concurrent.STM.TChan
import Control.Monad.STM

-- matrix helper functions

toGLmatrix :: M44 Float -> IO (GL.GLmatrix Float)
toGLmatrix = GL.newMatrix GL.RowMajor . (toList >=> toList)

-- rotation :: Float -> Float -> Float -> Float -> M44 Float
-- rotation angle x y z = identity & _m33 .~ fromQuaternion (Quaternion (cos (angle/2)) ((sin (angle/2) *) <$> normalize (V3 x y z)))

toVAO :: [(V3 Float, V2 Float, Float)] -- VBO data
      -> IO GL.VertexArrayObject -- VAO result
toVAO vboData = U.makeVAO $ do
  vbo <- U.makeBuffer GL.ArrayBuffer $ vboData >>= \(V3 a b c, V2 d e, f) -> [a,b,c,d,e,f]
  GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo

  let f = fromIntegral $ sizeOf (undefined :: Float)
      g n size totSize offset = do
        let attrib = GL.AttribLocation n
        GL.vertexAttribPointer attrib GL.$=
          (GL.ToFloat, GL.VertexArrayDescriptor size GL.Float totSize (U.offsetPtr (fromIntegral offset)))
        GL.vertexAttribArray attrib GL.$= GL.Enabled
  g 0 3 (6 * f) (0 * f)
  g 1 2 (6 * f) (3 * f)
  g 2 1 (6 * f) (5 * f)

-- This is pretty ugly
renderChunk :: V.Vector Block -> V3 Int -> M.Map (V3 Int) Chunk -> [(V3 Float, V2 Float, Float)]
renderChunk chunk pos world = force $ do
  v <- V3 <$> [0..chunkSize - 1] <*> [0..chunkSize - 1] <*> [0..chunkSize - 1]
  guard $ chunk V.! toPos v /= Air
  (v',face) <- zip [v & dir +~ mag | dir <- [_z,_y,_x], mag <- [1,-1]]
                   [northFace, southFace, topFace, bottomFace, eastFace, westFace]
  if (not (all (\x -> x >= 0 && x < chunkSize) v')) then
    case let f :: Lens' (V3 Int) Int -> (V3 Int,Maybe Chunk) -> (V3 Int,Maybe Chunk)
             f dir rest =
               if v' ^. dir < 0 then
                 (v' & dir +~ chunkSize, M.lookup (pos & dir -~ 1) world)
               else if v' ^. dir >= chunkSize then
                 (v' & dir -~ chunkSize, M.lookup (pos & dir +~ 1) world)
               else rest
         in f _x (f _y (f _z (error "this is impossible")))
         of
      (_,Nothing) -> []
      (v'',Just chunk') -> guard $ (chunk' ^. vec) V.! toPos v'' == Air
  else
    guard $ chunk V.! toPos v' == Air
  face & traverse . _1 +~ fmap fromIntegral v

toPos (V3 x y z) = x + y * xMax + z * xMax * yMax
  where
    xMax = chunkSize
    yMax = chunkSize

fromPos n = V3 x y z
  where
    (n',x) = quotRem n xMax
    (z,y) = quotRem n' yMax
    xMax = chunkSize
    yMax = chunkSize

switchCursor :: GLFW.Window -> IO ()
switchCursor win = do
  currentMode <- GLFW.getCursorInputMode win 
  GLFW.setCursorInputMode win $ case currentMode of
      GLFW.CursorInputMode'Normal -> GLFW.CursorInputMode'Disabled
      _ -> GLFW.CursorInputMode'Normal

keyCallback :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback win GLFW.Key'R _ GLFW.KeyState'Pressed _ = switchCursor win
keyCallback _ _ _ _ _ = return ()

gameLoop :: IO ()
gameLoop = withWindow 1280 720 "Jaro's minecraft ripoff [WIP]" $ \win -> do
  -- Disable the cursor
  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

  -- Generate a new game world
  game <- Game (Camera (V3 8 15 8) 0 0 5 0.001)
           <$> (realToFrac . fromJust <$> GLFW.getTime)
           <*> (fmap realToFrac . uncurry V2 <$> GLFW.getCursorPos win)
           <*> (newIORef M.empty)

  void $ flip runStateT game $ do
    -- vsync
    lift $ GLFW.swapInterval 0

    p <- lift $ do -- Shaders
      vs <- U.loadShader GL.VertexShader   "hypercube.v.glsl"
      fs <- U.loadShader GL.FragmentShader "hypercube.f.glsl"
      U.linkShaderProgram [vs, fs]

    lift $ do -- Load texture & generate mipmaps
      (Right texture) <- U.readTexture "img/stone.png"
      GL.textureBinding GL.Texture2D GL.$= Just texture
      GL.textureFilter GL.Texture2D GL.$= ((GL.Nearest,Nothing),GL.Nearest)
    
    lift $ GL.generateMipmap' GL.Texture2D

    -- Enable depth testing
    lift $ GL.depthFunc GL.$= Just GL.Less
            
    -- Get the shader uniforms
    [modelLoc,viewLoc,projLoc] <- lift $ 
      mapM (GL.get . GL.uniformLocation p) ["model","view","projection"]
            
    -- Set shader
    lift $ GL.currentProgram GL.$= Just p

    -- Set clear color
    lift $ GL.clearColor GL.$= GL.Color4 0.2 0.3 0.3 1

    chunkChan <- lift newTChanIO -- :: StateT Game IO (TChan (V3 Int, [(V3 Float,V2 Float)]))

    lift $ GL.cullFace GL.$= Just GL.Back

    lift $ GL.polygonMode GL.$= (GL.Fill, GL.Point)
  
    lift $ GLFW.setKeyCallback win (Just keyCallback)
  
    let 
      loop :: StateT Game IO ()
      loop = do
        shouldClose <- lift $ GLFW.windowShouldClose win
        unless shouldClose $ do
          
          lift $ GLFW.pollEvents

          deltaTime <- do
            currentFrame <- realToFrac . fromJust <$> lift GLFW.getTime
            deltaTime <- (currentFrame -) <$> use lastFrame
            lastFrame .= currentFrame
            return deltaTime

          -- lift $ print $ 1/deltaTime

          do -- Keyboard input
            let handle :: MonadIO m => (GLFW.KeyState -> Bool) -> GLFW.Key -> m () -> m ()
                handle f key action = f <$> liftIO (GLFW.getKey win key) >>= \x -> when x action

                isDown GLFW.KeyState'Pressed = True
                isDown GLFW.KeyState'Repeating = True
                isDown _ = False
                
                isPressed GLFW.KeyState'Pressed = True
                isPressed _ = False

            moveAmount <- (deltaTime *) <$> use (cam . speed)
            camGaze  <- use (cam . gaze )
            camSpeed <- use (cam . speed)
            camJaw   <- use (cam . jaw  )

            let
              forward = moveAmount *^ rotate (axisAngle (V3 0 1 0) camJaw) (V3 0 0 (-1))
              right   = moveAmount *^ rotate (axisAngle (V3 0 1 0) camJaw) (V3 1 0 0)
              up      = moveAmount *^ V3 0 1 0

            handle isDown GLFW.Key'W         $ cam . camPos += forward
            handle isDown GLFW.Key'A         $ cam . camPos -= right
            handle isDown GLFW.Key'S         $ cam . camPos -= forward
            handle isDown GLFW.Key'D         $ cam . camPos += right
            handle isDown GLFW.Key'Space     $ cam . camPos += up
            handle isDown GLFW.Key'LeftShift $ cam . camPos -= up

            handle isDown GLFW.Key'LeftControl  $ cam . speed .= 50
            handle (not . isPressed) GLFW.Key'LeftControl $ cam . speed .= 5

            lift $ handle isPressed GLFW.Key'Escape $ GLFW.setWindowShouldClose win True

          do -- Mouse input
            currentCursorPos <- fmap realToFrac . uncurry V2 <$> lift (GLFW.getCursorPos win)
            camSensitivity <- use (cam . sensitivity)
            prevCursorPos <- use cursorPos
            let cursorDelta = camSensitivity *^ (prevCursorPos - currentCursorPos)

            cursorPos   .= currentCursorPos
            cam . jaw   += cursorDelta ^. _x
            cam . pitch += cursorDelta ^. _y
          
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
              perspective (pi / 4) (fromIntegral width / fromIntegral height) 0.1 100
            GL.uniform projLoc GL.$= projMat

          do -- Draw the chunks
            lift (atomically (tryReadTChan chunkChan)) >>= \case
              Just (c,curC,vertices) -> do
                world <- use world
                worldVal <- lift $ readIORef world
                --lift $ print (curC,worldVal)
                vao <- lift $ toVAO vertices
                let chunk = (Chunk c (Right vao) (GL.drawArrays GL.Triangles 0 (fromIntegral $ length vertices)))
                lift (atomicModifyIORef world (\x -> (M.insert curC chunk x, ())))
              Nothing -> return ()

            curChunk <- fmap (floor . (/ fromIntegral chunkSize)) <$> use (cam . camPos)
            m <- lift . readIORef =<< use world
            let 
              render curC = 
                case M.lookup curC m of
                  Just (Chunk _ (Right vao) f) -> lift $ do
                    U.withVAO vao $ do
                      model <- toGLmatrix (identity & translation +~ (realToFrac chunkSize *^ (fmap fromIntegral curC)))
                      GL.uniform modelLoc GL.$= model
                      f
                  Just (Chunk _ (Left _) _) -> return ()
                  Nothing -> use world >>= \world -> lift $ void $ forkIO $ do
                    do
                      let vec = (V.generate (chunkSize ^ 3) $ generatingF . (+ (chunkSize *^ curC)) . fromPos)
                      worldVal <- readIORef world
                      atomically . writeTChan chunkChan $ (vec, curC, renderChunk vec curC worldVal)
                      atomicModifyIORef world $ \x -> (M.insert curC (Chunk vec (Left undefined) (return ())) x, ())
                    worldVal <- readIORef world
                    sequence_ $ do
                      v' <- [curC & dir +~ mag | dir <- [_x,_y,_z], mag <- [1,-1]]
                      chunk <- maybeToList (M.lookup v' worldVal)
                      return $ atomically $ writeTChan chunkChan $ (chunk ^. vec,v',renderChunk (chunk ^. vec) v' worldVal)
              r = renderDistance 
            mapM render [curChunk + V3 x y z | x <- [-r..r], y <- [-2..2], z <- [-r..r], x^2 + z^2 <= r^2]

          -- Swap the buffers (aka draw the final image to the screen)
          lift $ GLFW.swapBuffers win

          -- Recurse
          loop
    loop

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

