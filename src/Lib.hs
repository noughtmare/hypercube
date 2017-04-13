{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE BangPatterns #-} 

module Lib where

import qualified Debug.Trace as D

import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Default 
import Control.Lens
import Control.Concurrent

import Data.List
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

import Foreign.Ptr

import qualified Data.Map as M
import Control.DeepSeq

import Control.Concurrent.STM.TChan
import Control.Monad.STM

-- matrix helper functions

toGLmatrix :: M44 Float -> IO (GL.GLmatrix Float)
toGLmatrix = GL.newMatrix GL.RowMajor . (toList >=> toList)

rotation :: Float -> Float -> Float -> Float -> M44 Float
rotation angle x y z = identity & _m33 .~ fromQuaternion (Quaternion (cos (angle/2)) ((sin (angle/2) *) <$> normalize (V3 x y z)))

-- Data

toVAO :: [(V3 Float, V2 Float)] -- VBO data
      -> IO GL.VertexArrayObject -- VAO result
toVAO vboData = U.makeVAO $ do
  vbo <- U.makeBuffer GL.ArrayBuffer $ vboData >>= \(V3 a b c, V2 d e) -> [a,b,c,d,e]
  GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo

  let f = fromIntegral $ sizeOf (undefined :: Float) in do
    let attrib = GL.AttribLocation 0 in do
      GL.vertexAttribPointer attrib GL.$=
        (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (5 * f) U.offset0)
      GL.vertexAttribArray attrib GL.$= GL.Enabled

    let attrib = GL.AttribLocation 1 in do
      GL.vertexAttribPointer attrib GL.$=
        (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (5 * f) (U.offsetPtr (fromIntegral $ 3 * f)))
      GL.vertexAttribArray attrib GL.$= GL.Enabled

northFace = 
  [ (V3 1 1 1, V2 0 0) -- top    right
  , (V3 0 1 1, V2 1 0) -- top    left
  , (V3 0 0 1, V2 1 1) -- bottom left
  , (V3 0 0 1, V2 1 1) -- bottom left
  , (V3 1 0 1, V2 0 1) -- bottom right
  , (V3 1 1 1, V2 0 0) -- top    right
  ]

southFace = 
  [ (V3 0 0 0, V2 1 1) -- bottom left
  , (V3 0 1 0, V2 1 0) -- top    left
  , (V3 1 1 0, V2 0 0) -- top    right
  , (V3 1 1 0, V2 0 0) -- top    right
  , (V3 1 0 0, V2 0 1) -- bottom right
  , (V3 0 0 0, V2 1 1) -- bottom left
  ]

eastFace =
  [ (V3 1 1 0, V2 0 0) -- top    right
  , (V3 1 1 1, V2 1 0) -- top    left
  , (V3 1 0 1, V2 1 1) -- bottom left
  , (V3 1 0 1, V2 1 1) -- bottom left
  , (V3 1 0 0, V2 0 1) -- bottom right
  , (V3 1 1 0, V2 0 0) -- top    right
  ]
  
westFace = 
  [ (V3 0 0 1, V2 1 1) -- bottom left
  , (V3 0 1 1, V2 1 0) -- top    left
  , (V3 0 1 0, V2 0 0) -- top    right
  , (V3 0 1 0, V2 0 0) -- top    right
  , (V3 0 0 0, V2 0 1) -- bottom right
  , (V3 0 0 1, V2 1 1) -- bottom left
  ]

topFace =
  [ (V3 0 1 0, V2 1 1) -- bottom left
  , (V3 0 1 1, V2 1 0) -- top    left
  , (V3 1 1 1, V2 0 0) -- top    right
  , (V3 1 1 1, V2 0 0) -- top    right
  , (V3 1 1 0, V2 0 1) -- bottom right
  , (V3 0 1 0, V2 1 1) -- bottom left
  ]

bottomFace =
  [ (V3 1 0 1, V2 0 0) -- top    right
  , (V3 0 0 1, V2 1 0) -- top    left
  , (V3 0 0 0, V2 1 1) -- bottom left
  , (V3 0 0 0, V2 1 1) -- bottom left
  , (V3 1 0 0, V2 0 1) -- bottom right
  , (V3 1 0 1, V2 0 0) -- top    right
  ]

data Camera 
  = Camera
  { _camPos :: !(V3 Float)
  , _jaw :: !Float
  , _pitch :: !Float
  , _speed :: !Float
  , _sensitivity :: !Float
  } deriving (Show)

makeLensesFor [("_camPos","camPos"),("_jaw","jaw"),("_speed","speed"),("_sensitivity","sensitivity")] ''Camera

-- | "Advanced" lensing (we check bounds inside the setter)
pitch :: Lens' Camera Float
pitch = lens _pitch setter
  where
    setter cam x
      | abs x > pi/2 - epsilon = cam {_pitch = (signum x) * (pi / 2 - epsilon)}
      | otherwise              = cam {_pitch = x}
    epsilon = 0.005

gaze :: Getter Camera (V3 Float)
gaze = to $ gaze' . (view jaw &&& view pitch)
  where
    gaze' (jaw,pitch) 
      = rotate (axisAngle (V3 0 1 0) jaw)
      $ rotate (axisAngle (V3 1 0 0) pitch) 
      $ V3 0 0 (-1)

data Game 
  = Game
  { _cam       :: !Camera
  , _lastFrame :: !Float
  , _cursorPos :: !(V2 Float)
  , _world     :: !(IORef World)
  }

type World = M.Map (V3 Int) Chunk
data Chunk = Chunk
  { _vec :: !(V.Vector Block)
  , _mesh :: Either (IO GL.VertexArrayObject) GL.VertexArrayObject
  , _draw :: IO ()
  }

instance Show Chunk where
  show _ = "chunk"

data Block
  = Stone
  | Air
  deriving (Eq, Ord, Show, Enum)

makeLenses ''Game
makeLenses ''Chunk

-- This is slightly ugly
renderChunk :: V.Vector Block -> V3 Int -> M.Map (V3 Int) Chunk -> (V.Vector Block, V3 Int, [(V3 Float, V2 Float)])
renderChunk chunk pos world =
  let 
    !vertices = force $ do
      v <- V3 <$> [0..chunkSize - 1] <*> [0..chunkSize - 1] <*> [0..chunkSize - 1]
      guard $ chunk V.! toPos v /= Air
      (v',face) <- zip (map (+v) [V3 0 0 1,V3 0 0 (-1),V3 0 1 0,V3 0 (-1) 0, V3 1 0 0, V3 (-1) 0 0]) 
                       [northFace, southFace, topFace, bottomFace, eastFace, westFace]
      if (not (all (\x -> x >= 0 && x < chunkSize) v')) then
        case ( 
          if v' ^. _x < 0 then
            (v' & _x +~ chunkSize, M.lookup (pos & _x -~ 1) world)
          else if v' ^. _x >= chunkSize then
            (v' & _x -~ chunkSize, M.lookup (pos & _x +~ 1) world)
          else if v' ^. _y < 0 then
            (v' & _y +~ chunkSize, M.lookup (pos & _y -~ 1) world)
          else if v' ^. _y >= chunkSize then
            (v' & _y -~ chunkSize, M.lookup (pos & _y +~ 1) world)
          else if v' ^. _z < 0 then
            (v' & _z +~ chunkSize, M.lookup (pos & _z -~ 1) world)
          else if v' ^. _z >= chunkSize then
            (v' & _z -~ chunkSize, M.lookup (pos & _z +~ 1) world)
          else 
            error "This is impossible") of
          (_,Nothing) -> []
          (v'',Just chunk') -> guard $ (chunk' ^. vec) V.! toPos v'' == Air
      else 
        guard $ chunk V.! toPos v' == Air 
      face & traverse . _1 +~ fmap fromIntegral v
  in (chunk,pos,vertices)

-- Code

chunkSize :: Int
chunkSize = 16

renderDistance :: Int
renderDistance = 5

data Program = Program GL.Program GL.AttribLocation GL.BufferObject

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

generatingF :: V3 Int -> Block
generatingF (V3 x y z)
  | (x-8) ^ 2 + y ^ 2 + (z-8) ^ 2 < 128 = Stone
  | y < 2 = Stone
  | otherwise = Air

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
      (Right texture) <- U.readTexture "stone.png"
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

                isPressed GLFW.KeyState'Pressed = True
                isPressed GLFW.KeyState'Repeating = True
                isPressed _ = False

            moveAmount <- (deltaTime *) <$> use (cam . speed)
            camGaze  <- use (cam . gaze )
            camSpeed <- use (cam . speed)
            camJaw   <- use (cam . jaw  )

            let
              forward = moveAmount *^ rotate (axisAngle (V3 0 1 0) camJaw) (V3 0 0 (-1))
              right   = moveAmount *^ rotate (axisAngle (V3 0 1 0) camJaw) (V3 1 0 0)
              up      = moveAmount *^ V3 0 1 0

            handle isPressed GLFW.Key'W         $ cam . camPos += forward
            handle isPressed GLFW.Key'A         $ cam . camPos -= right
            handle isPressed GLFW.Key'S         $ cam . camPos -= forward
            handle isPressed GLFW.Key'D         $ cam . camPos += right
            handle isPressed GLFW.Key'Space     $ cam . camPos += up
            handle isPressed GLFW.Key'LeftShift $ cam . camPos -= up
            handle isPressed GLFW.Key'R         $ lift $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal

            handle isPressed GLFW.Key'LeftControl  $ cam . speed .= 50
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
                      model <- toGLmatrix (identity & translation +~ (16 *^ (fmap fromIntegral curC)))
                      GL.uniform modelLoc GL.$= model
                      f
                  Just (Chunk _ (Left _) _) -> return ()
                  Nothing -> use world >>= \world -> lift $ void $ forkIO $ do
                    do
                      let vec = (V.generate (chunkSize ^ 3) $ generatingF . (+ (16 *^ curC)) . fromPos)
                      worldVal <- readIORef world
                      atomically . writeTChan chunkChan $ renderChunk vec curC worldVal
                      atomicModifyIORef world $ \x -> (M.insert curC (Chunk vec (Left undefined) (return ())) x, ())
                    worldVal <- readIORef world
                    sequence_ $ do
                      v' <- [curC & dir +~ mag | dir <- [_x,_y,_z], mag <- [1,-1]]
                      chunk <- maybeToList (M.lookup v' worldVal)
                      return $ atomically $ writeTChan chunkChan $ renderChunk (chunk ^. vec) v' worldVal
              r = renderDistance 
            mapM render [curChunk + V3 x y z | x <- [-r..r], y <- [-r..r], z <- [-r..r], x^2 + y^2 + z^2 <= r^2]

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

