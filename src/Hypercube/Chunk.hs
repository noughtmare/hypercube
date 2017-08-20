{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}

{-|
Module      : Hypercube.Chunk
Description : 3D Vectors
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains all code involving Chunks in Hypercube (except for the type declaration).
-}

module Hypercube.Chunk
  ( newChunk
  , renderChunk
  ) where

import Hypercube.Chunk.Faces
import Hypercube.Config (chunkSize, generatingF)
import Hypercube.Types
import Hypercube.Util

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Graphics.Rendering.OpenGL hiding (get)
import Linear
import Control.Monad
import Foreign.Ptr (intPtrToPtr)
import Control.Concurrent
import Control.Lens
import Data.Word (Word8)
import Control.Monad.Trans.State
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (liftIO)

toPos :: V3 Int -> Int
toPos (V3 x y z) = x + chunkSize * y + chunkSize * chunkSize * z

fromPos :: Int -> V3 Int
fromPos n =
    let (n',x) = n `quotRem` chunkSize
        (z,y)  = n' `quotRem` chunkSize
    in V3 x y z

newChunk :: V3 Int -> TChan (V.Vector (V4 Word8), BufferObject, MVar Int)-> IO Chunk
newChunk v chan = do
  vbo <- genObjectName
  var <- newEmptyMVar
  return $ Chunk
    (V.generate (chunkSize ^ (3 :: Int)) (generatingF . (+ chunkSize *^ v) . fromPos))
    vbo
    0
    True
    v
    False
    chan
    var

updateChunk :: StateT Chunk IO ()
updateChunk = do
  isChanging <- use chunkChanging
  var <- use chunkIsLoaded
  unless isChanging $ do
    chunkChanging .= True
    blk  <- use chunkBlk
    chan <- use chunkChan
    vbo  <- use chunkVbo
    pos  <- use chunkPos
    -- We're (ab)using laziness here to postpone the actual surface extraction
    -- until the chunk data gets uploaded to the graphics card (in Game.hs)
    -- I don't think this is the best way to do this, but it works.
    liftIO $ atomically $ writeTChan chan (extractSurface blk pos,vbo,var)

  mayTemp <- liftIO $ tryTakeMVar var
  case mayTemp of
    Nothing -> return ()
    Just l -> do
      chunkChanged .= False
      chunkChanging .= False
      chunkElements .= l

extractSurface :: V.Vector Block -> V3 Int -> V.Vector (V4 Word8)
extractSurface blk pos = do
  (p,_) <- V.filter ((/= Air) . snd) $ V.indexed blk
  let v = fromPos p
  (v',face) <- V.fromList $ zip
                 [ v & dir +~ mag | dir <- [_z,_y,_x], mag <- [1,-1]]
                 [ northFace & traverse . _w +~ 1
                 , southFace & traverse . _w +~ 1
                 , topFace
                 , bottomFace
                 , eastFace & traverse . _w +~ 1
                 , westFace & traverse . _w +~ 1
                 ]
  guard $ (if all (liftA2 (&&) (>= 0) (< chunkSize)) v'
            then blk V.! toPos v'
            else generatingF (v' + chunkSize *^ pos)) == Air -- hack
  face & traverse +~ (0 & _xyz .~ fmap fromIntegral v)


renderChunk :: UniformLocation -> StateT Chunk IO ()
renderChunk modelLoc = do
  isChanged <- use chunkChanged
  when isChanged updateChunk
  n <- use chunkElements
  when (n > 0) $ do
    use chunkVbo >>= (bindBuffer ArrayBuffer $=) . Just
    pos <- use chunkPos

    liftIO $ do
      model <- toGLmatrix $ identity & translation +~
        (fromIntegral chunkSize *^ fmap fromIntegral pos)
      uniform modelLoc $= model

      vertexAttribPointer (AttribLocation 0) $=
        (ToFloat, VertexArrayDescriptor 4 Byte 0 (intPtrToPtr 0))
      vertexAttribArray (AttribLocation 0) $= Enabled

      drawArrays Triangles 0 $ fromIntegral n

