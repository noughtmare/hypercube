{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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

import qualified Data.Vector as V
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
  :: IORef [V3 Int]
  -> TChan (V3 Int, Chunk, VS.Vector (V4 Int8))
  -> IO ()
startChunkManager todo chan = void $ forkIO $ forever $
  atomicModifyIORef' todo (maybe ([],Nothing) (\(h,t) -> (t,Just h)) . uncons)
    >>= maybe (return ()) (\pos -> do
          -- atomicModifyIORef' busy (\a -> (pos:a,())
          --putStrLn ("chunkMan: " ++ show pos)
          chunk <- newChunk pos
          atomically $ writeTChan chan (pos, chunk, extractSurface pos (chunk ^. chunkBlk)))

toPos :: V3 Int -> Int
toPos (V3 x y z) = x + chunkSize * y + chunkSize * chunkSize * z

fromPos :: Int -> V3 Int
fromPos n =
    let (n',x) = n `quotRem` chunkSize
        (z,y)  = n' `quotRem` chunkSize
    in V3 x y z

newChunk :: V3 Int -> IO Chunk
newChunk pos = do
  let blk = V.generate (chunkSize ^ (3 :: Int)) (generatingF . (+ chunkSize *^ pos) . fromPos)
  blk `deepseq` return (Chunk blk undefined undefined 0 True)

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

extractSurface :: V3 Int -> V.Vector Block -> VS.Vector (V4 Int8)
extractSurface pos blk = VS.fromList $ do
      v <- liftA3 V3 [0..chunkSize - 1] [0..chunkSize - 1] [0..chunkSize - 1]
      d <- [North .. Bottom] 
      let v' = dir d v
      guard (blk V.! toPos v /= Air)
      guard $ (Air ==) $ if all (\x -> 0 <= x && x < chunkSize) v'
        then blk V.! toPos v'
        else generatingF ((chunkSize *^ pos) + v')
      face d & traverse +~ (0 & _xyz .~ fmap fromIntegral v 
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

