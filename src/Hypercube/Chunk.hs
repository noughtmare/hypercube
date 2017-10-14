{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Graphics.Rendering.OpenGL hiding (get)
import Linear
import Control.Monad
import Foreign.Ptr
import Control.Concurrent
import Control.Lens
import Data.Int (Int8)
import Control.Monad.Trans.State
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Applicative (liftA3)
import Control.Monad.IO.Class (liftIO)
import Data.Function
import Data.IORef
import Control.DeepSeq

startChunkManager 
  :: IORef [V3 Int]
  -> TChan (V3 Int, Chunk, VS.Vector (V4 Int8))
  -> IO ()
startChunkManager todo chan = void $ forkIO $ fix $ \manageChunks -> do
  chunk <- atomicModifyIORef' todo $ \chunks -> 
    case uncons chunks of
      Nothing -> (chunks,Nothing)
      Just (chunk,rest) -> (rest,Just chunk)
  case chunk of 
    Nothing -> manageChunks
    Just pos -> do
      -- atomicModifyIORef' busy (\a -> (pos:a,())
      --putStrLn ("chunkMan: " ++ show pos)
      chunk <- newChunk pos
      atomically $ writeTChan chan (pos, chunk, extractSurface (chunk ^. chunkBlk))
      manageChunks

toPos :: V3 Int -> Int
toPos (V3 x y z) = x + chunkSize * y + chunkSize * chunkSize * z

fromPos :: Int -> V3 Int
fromPos n =
    let (n',x) = n `quotRem` chunkSize
        (z,y)  = n' `quotRem` chunkSize
    in V3 x y z

newChunk :: V3 Int -> IO Chunk
newChunk pos = do
  --putStrLn ("newChunk: " ++ show pos)
  let blk = V.generate (chunkSize ^ (3 :: Int)) (generatingF . (+ chunkSize *^ pos) . fromPos)
  blk `deepseq` return (Chunk blk undefined undefined 0 True)

updateChunk :: V3 Int -> StateT Chunk IO ()
updateChunk pos = do
--   isChanging <- use chunkChanging
--   var <- use chunkIsLoaded
--   unless isChanging $ do
--     chunkChanging .= True
--     blk  <- use chunkBlk
--     chan <- use chunkChan
--     vbo  <- use chunkVbo
--     -- We're (ab)using laziness here to postpone the actual surface extraction
--     -- until the chunk data gets uploaded to the graphics card (in Game.hs)
--     -- I don't think this is the best way to do this, but it works.
--     x <- liftIO $ (\(_,b,c) -> (b,c)) <$> extractSurface pos blk
--     liftIO $ atomically $ writeTChan chan (x,vbo,var)
-- 
--   mayTemp <- liftIO $ tryTakeMVar var
--   case mayTemp of
--     Nothing -> return ()
--     Just l -> do
--       chunkChanged .= False
--       chunkChanging .= False
--       chunkElements .= l
  return ()

east, west, top, bottom, north, south :: V3 Int -> V3 Int
east   = _x +~ 1
west   = _x -~ 1
top    = _y +~ 1
bottom = _y -~ 1
north  = _z +~ 1
south  = _z -~ 1

data Direction = North | East | South | West | Top | Bottom
  deriving (Show, Eq, Enum)

dir :: Direction -> V3 Int -> V3 Int
dir North  = north
dir East   = east
dir South  = south
dir West   = west
dir Top    = top
dir Bottom = bottom

face :: Direction -> [V4 Int8]
face North  = northFace
face East   = eastFace
face South  = southFace
face West   = westFace
face Top    = topFace
face Bottom = bottomFace

extractSurface :: V.Vector Block -> VS.Vector (V4 Int8)
extractSurface blk
  | V.all (== blk V.! 0) blk = VS.empty
  | otherwise = VS.fromList $ do
      v <- liftA3 V3 [0..15] [0..15] [0..15]
      d <- [North .. Bottom] 
      let v' = dir d v
      guard (blk V.! toPos v /= Air 
          && all (\x -> x >= 0 && x < 16) v' 
          && blk V.! toPos v' == Air)
      face d & traverse +~ (0 & _xyz .~ fmap fromIntegral v 
                              & _w   .~ if d `elem` [Top,Bottom] then 0 else 1)

renderChunkTheOldWay :: V3 Int -> UniformLocation -> StateT Chunk IO ()
renderChunkTheOldWay pos modelLoc = do
  --isChanged <- use chunkChanged
  --when isChanged (updateChunk pos)
  n <- use chunkElements
  when (n > 0) $ do
    use chunkVbo >>= (bindBuffer ArrayBuffer $=) . Just
    liftIO $ do
      model <- toGLmatrix $ identity & translation +~
        (fromIntegral <$> chunkSize *^ pos)
      uniform modelLoc $= model

      vertexAttribPointer (AttribLocation 0) $=
        (KeepIntegral, VertexArrayDescriptor 4 Byte 0 (intPtrToPtr 0))

      vertexAttribArray (AttribLocation 0) $= Enabled
      drawArrays Triangles 0 $ fromIntegral n
      vertexAttribArray (AttribLocation 0) $= Disabled

renderChunk :: V3 Int -> UniformLocation -> StateT Chunk IO ()
renderChunk pos modelLoc = do
  --isChanged <- use chunkChanged
  --when isChanged (updateChunk pos)
  n <- use chunkElements
  when (n > 0) $ do
    use chunkVao >>= (bindVertexArrayObject $=) . Just
    liftIO $ do
      model <- toGLmatrix $ identity & translation +~
        (fromIntegral <$> chunkSize *^ pos)
      uniform modelLoc $= model

      drawArrays Triangles 0 $ fromIntegral n

