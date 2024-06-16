{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dno-suppress-type-signatures -ddump-to-file #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE Strict #-}

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
  , startChunkManager
  ) where

import Hypercube.Chunk.Faces
import Hypercube.Config (chunkSize, generatingF)
import Hypercube.Types
import Hypercube.Util

import qualified Data.Vector.Storable as VS
import Graphics.Rendering.OpenGL hiding (get)
import Linear
import Control.Monad
import Control.Concurrent
import Control.Lens
import Data.Int (Int8)
import Control.Monad.Trans.State
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Applicative (liftA3)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Control.DeepSeq (deepseq)

startChunkManager
  :: TChan (V3 Int)
  -> TChan ChunkResponse
  -> IO ()
startChunkManager todo chan = void $ forkIO $ forever $ do
  pos <- atomically (readTChan todo)
  chunk <- newChunk pos
  let !cr = ChunkResponse pos chunk $ extractSurface pos $ chunk ^. chunkBlk
  atomically $ writeTChan chan cr

toPos :: V3 Int -> Int
toPos (V3 x y z) = x + chunkSize * y + chunkSize * chunkSize * z

fromPos :: Int -> V3 Int
fromPos n =
    let (n',x) = n `quotRem` chunkSize
        (z,y)  = n' `quotRem` chunkSize
    in V3 x y z

newChunk :: V3 Int -> IO Chunk
{-# SCC newChunk #-}
newChunk pos = do
  let blk = VS.generate (chunkSize ^ (3 :: Int)) (generatingF . (+ chunkSize *^ pos) . fromPos)
  return (Chunk blk undefined undefined 0 True)

data Direction = North | East | South | West | Top | Bottom
  deriving (Show, Eq, Enum)

dir :: Direction -> V3 Int -> V3 Int
dir = \case
  East   -> _x +~ 1
  West   -> _x -~ 1
  Top    -> _y +~ 1
  Bottom -> _y -~ 1
  North  -> _z +~ 1
  South  -> _z -~ 1

face :: Direction -> [V4 Int8]
face = \case
  East   -> eastFace
  West   -> westFace
  Top    -> topFace
  Bottom -> bottomFace
  North  -> northFace
  South  -> southFace

-- extractOne :: V3 Int -> VS.Vector Block -> (VS.Vector (V4 Int8), VS.Vector (V4 Int8))
-- extractOne = _

extractSurface :: V3 Int -> VS.Vector Block -> VS.Vector (V4 Int8)
{-# SCC extractSurface #-}
{-# NOINLINE extractSurface #-}
extractSurface pos blk = VS.fromList $ do
  v <- liftA3 V3 [0..chunkSize - 1] [0..chunkSize - 1] [0..chunkSize - 1]
  d <- [North .. Bottom] 
  let v' = dir d v
  guard (blk VS.! toPos v /= Air)
  guard $ (Air ==) $ if all (\x -> 0 <= x && x < chunkSize) v'
    then blk VS.! toPos v'
    else generatingF ((chunkSize *^ pos) + v')
  face d & traverse +~ 
    (0
      & _xyz .~ fmap fromIntegral v 
      & _w   .~ if d `elem` [Top,Bottom] then 0 else 1)

renderChunk :: V3 Int -> UniformLocation -> StateT Chunk IO ()
renderChunk pos modelLoc = do
  n <- use chunkElements
  when (n > 0) $ do
    use chunkVao >>= (bindVertexArrayObject $=) . Just
    liftIO $ do
      model <- toGLmatrix $ identity & translation +~
        (fromIntegral <$> chunkSize *^ pos)
      uniform modelLoc $= model

      drawArrays Triangles 0 $ fromIntegral n

