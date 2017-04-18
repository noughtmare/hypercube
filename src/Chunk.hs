{-# LANGUAGE TupleSections #-}

module Chunk where
{-
  ( newChunk
  , deleteChunk
  -- , withChunk
  , getBlock
  , setBlock
  , modifyBlock
  -- , updateChunk
  , renderChunk
  ) where

import qualified Debug.Trace as D

import Util
import Types (Chunk (..), Block (..))
import Config (chunkSize, generatingF)
import Faces

import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Storable as SV
import Data.Vector.Mutable (IOVector)
import Graphics.Rendering.OpenGL
import Linear
import Control.Monad
import Data.IORef
import Control.Lens ((+~), (-~), (&), (.~), (^.))
import Foreign.Storable (sizeOf)
import Foreign.Ptr (intPtrToPtr)
import Data.Foldable (toList)
import qualified Data.Vector as V (generate, thaw)
import Control.Concurrent
import qualified Data.Map as M
import Data.Maybe
import System.CPUTime
import qualified Graphics.UI.GLFW as GLFW

toPos :: V3 Int -> Int
toPos (V3 x y z) =
  let res = x + chunkSize * y + (chunkSize ^ (2::Int)) * z
  in if res < 0 || res >= chunkSize ^ (3::Int)
       then error "Out of bounds"
       else res

fromPos :: Int -> V3 Int
fromPos n
  | n < 0 || n >= chunkSize ^ (3 :: Int) = error "Position out of bounds"
  | otherwise =
    let (n',x) = n `quotRem` chunkSize
        (z,y)  = n' `quotRem` chunkSize
    in V3 x y z

newChunk :: V3 Int -> IORef (M.Map (V3 Int) Chunk) -> IO Chunk
newChunk v w = do
  c <-
    Chunk <$> V.thaw (V.generate (chunkSize ^ 3) (generatingF . (+ chunkSize *^ v) . fromPos))
          <*> genObjectName
          <*> newIORef 0
          <*> newIORef True
          <*> pure v
          <*> pure w
  setChunkChanged c
  return c

deleteChunk :: Chunk -> IO ()
deleteChunk c = deleteObjectName $ chunkVbo c

-- withChunk :: (Chunk -> IO ()) -> IO ()
-- withChunk f = do
--   c <- newChunk
--   f c
--   deleteChunk c
--
setChunkChanged :: Chunk -> IO ()
setChunkChanged c = do
  m <- readIORef (chunkWorld c)
  sequence_ $ do
    v <- [V3 0 0 0, V3 0 0 1,V3 0 0 (-1), V3 0 1 0, V3 0 (-1) 0, V3 1 0 0, V3 (-1) 0 0]
    c' <- maybeToList (M.lookup (v + chunkPos c) m)
    return $ writeIORef (chunkChanged c') True

getBlock :: Chunk -> V3 Int -> IO Block
getBlock c v = V.unsafeRead (chunkBlk c) (toPos v)

setBlock :: Chunk -> V3 Int -> Block -> IO ()
setBlock c v b = do
  V.unsafeWrite (chunkBlk c) (toPos v) b
  setChunkChanged c
  -- writeIORef (chunkChanged c) True

modifyBlock :: Chunk -> V3 Int -> (Block -> Block) -> IO ()
modifyBlock c v f = do
  V.unsafeModify (chunkBlk c) f (toPos v)
  setChunkChanged c
  --writeIORef (chunkChanged c) True

updateChunk :: Chunk -> IO ()
updateChunk c = do
  t0 <- fromJust <$> GLFW.getTime
  writeIORef (chunkChanged c) False
  m <- readIORef $ chunkWorld c
  let [n,s,e,w,t,b] = [V3 0 0 1, V3 0 0 (-1), V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0]
        >>= \v -> return (M.lookup (v + chunkPos c) m)
  vertices <- do
    blocks <- sequence $ do
      v <- V3 <$> [0..chunkSize - 1] <*> [0..chunkSize -1] <*> [0..chunkSize - 1]
      return $ getBlock c v >>= return . (,v)
    neighbors <- sequence $ do
      (block,v) <- blocks
      guard $ block /= Air
      (v',face) <- zip [v & dir +~ mag | dir <- [_z,_y,_x], mag <- [1,-1]]
                       [ northFace & traverse . _w +~ 1
                       , southFace & traverse . _w +~ 1
                       , topFace, bottomFace
                       , eastFace & traverse . _w +~ 1
                       , westFace & traverse . _w +~ 1
                       ]
      if all (\x -> x >= 0 && x < chunkSize) v' then
        return $ getBlock c v' >>= \n -> return (n,face,v)
      else if v' ^. _x >= chunkSize && isJust e then
        return $ getBlock (fromJust e) (v' & _x -~ chunkSize) >>=  \n -> return (n,face,v)
      else if v' ^. _x < 0 && isJust w then
        return $ getBlock (fromJust w) (v' & _x +~ chunkSize) >>=  \n -> return (n,face,v)
      else if v' ^. _y >= chunkSize && isJust t then
        return $ getBlock (fromJust t) (v' & _y -~ chunkSize) >>=  \n -> return (n,face,v)
      else if v' ^. _y < 0 && isJust b then
        return $ getBlock (fromJust b) (v' & _y +~ chunkSize) >>=  \n -> return (n,face,v)
      else if v' ^. _z >= chunkSize && isJust n then
        return $ getBlock (fromJust n) (v' & _z -~ chunkSize) >>=  \n -> return (n,face,v)
      else if v' ^. _z < 0 && isJust s then
        return $ getBlock (fromJust s) (v' & _z +~ chunkSize) >>=  \n -> return (n,face,v)
      else []
    return $ do
      (n,face,v) <- neighbors
      guard $ n == Air
      face & traverse +~ (0 & _xyz .~ fmap fromIntegral v)

  --t1 <- fromJust <$> GLFW.getTime

  let vAmount = length vertices
  when (vAmount /= 0) $ do
    writeIORef (chunkElements c) vAmount
    bindBuffer ArrayBuffer $= Just (chunkVbo c)
    SV.unsafeWith (SV.fromList (vertices)) $ \ptr ->
      bufferData ArrayBuffer $= (fromIntegral (vAmount * 4),ptr,StaticDraw)

  --t2 <- fromJust <$> GLFW.getTime

  -- putStrLn $ "t1: " ++ show ((t1 - t0))
  -- putStrLn $ "t2: " ++ show ((t2 - t1))

renderChunk :: Chunk -> UniformLocation -> IO ()
renderChunk c modelLoc = do
  isChunkChanged <- readIORef $ chunkChanged c
  when isChunkChanged $ do
    updateChunk c
  n <- readIORef $ chunkElements c
  when (n /= 0) $ do
    cullFace $= Just Back
    depthFunc $= Just Less

    bindBuffer ArrayBuffer $= Just (chunkVbo c)
    vertexAttribPointer (AttribLocation 0) $=
      (ToFloat, VertexArrayDescriptor 4 Byte 0 (intPtrToPtr 0))
    vertexAttribArray (AttribLocation 0) $= Enabled

    model <- toGLmatrix $ identity & translation +~ (fromIntegral chunkSize *^ (fmap fromIntegral $ chunkPos c))
    uniform modelLoc $= model

    chunkElements' <- readIORef $ chunkElements c
    drawArrays Triangles 0 $ fromIntegral chunkElements'

-}
