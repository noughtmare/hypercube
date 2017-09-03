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
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Storable as SV
import Graphics.Rendering.OpenGL hiding (get)
import Linear
import Control.Monad
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal hiding (void)
import Control.Concurrent
import Control.Lens
import Data.Word (Word8)
import Control.Monad.Trans.State
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Applicative (liftA2)
import Control.Monad.IO.Class (liftIO)
import Control.Arrow (first)
import Data.Function
import Data.IORef
import Control.DeepSeq
import qualified Data.Set as S

{-
minMaxBy :: (a -> a -> Ordering) -> a -> a -> (a,a)
minMaxBy cmp a b = case cmp a b of
  GT -> (b,a)
  _  -> (a,b)

unconsMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe (a,[a])
unconsMinimumBy _ [] = Nothing
unconsMinimumBy cmp (x:xs) = Just (unconsMinimumBy' x [] xs)
  where
    unconsMinimumBy' m r [] = (m,r)
    unconsMinimumBy' m r (x:xs) = 
      let (mi,ma) = minMaxBy cmp m x
      in unconsMinimumBy' mi (ma:r) xs
-}

startChunkManager 
  :: IORef [V3 Int]
  -> TChan (V3 Int, Chunk, Ptr CUChar)
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
      putStrLn ("chunkMan: " ++ show pos)
      (Chunk blk vbo 0 True) <- newChunk pos
      (x,n) <- extractSurface pos blk
      atomically $ writeTChan chan (pos, Chunk blk vbo n True, x)
      manageChunks

toPos :: V3 Int -> Int
toPos (V3 x y z) = x + chunkSize * y + chunkSize * chunkSize * z

fromPos :: Int -> V3 Int
fromPos n =
    let (n',x) = n `quotRem` chunkSize
        (z,y)  = n' `quotRem` chunkSize
    in V3 x y z

newChunk :: V3 Int -> IO Chunk
newChunk v = do
  vbo <- genObjectName
  let blk = V.generate (chunkSize ^ (3 :: Int)) (generatingF . (+ chunkSize *^ v) . fromPos)
  blk `deepseq` return (Chunk blk vbo 0 True)

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

-- insideChunk :: Int -> Bool
-- insideChunk n 
--   | n < 0 || n >= (chunkSize ^ 3) = False
--   | otherwise = True
-- 
-- east, west, top, bottom, north, south :: Int -> Int
-- east   = toPos . (_x +~ 1) . fromPos
-- west   = toPos . (_x -~ 1) . fromPos
-- top    = toPos . (_y +~ 1) . fromPos
-- bottom = toPos . (_y -~ 1) . fromPos
-- north  = toPos . (_z +~ 1) . fromPos
-- south  = toPos . (_z -~ 1) . fromPos

-- extractSurface :: V.Vector Block -> V3 Int -> V.Vector (V4 Word8)
-- extractSurface blk pos = V.fromList $ do
--   (v,blockType) <- V.toList
--                  $ V.filter (\x -> snd x /= Air)
--                  $ V.indexed blk
--   (v',face) <- filter ((\v' -> insideChunk v' && blk V.! v' == Air) . fst) (faces v)
--   face & traverse . _xyz +~ fmap fromIntegral (fromPos v)
--   where
--     faces :: Int -> [(Int, [V4 Word8])]
--     faces v = 
--       [ (east v, eastFace & traverse . _w +~ 1)
--       , (west v, westFace & traverse . _w +~ 1)
--       , (top v, topFace)
--       , (bottom v, bottomFace)
--       , (north v, northFace & traverse . _w +~ 1)
--       , (south v, southFace & traverse . _w +~ 1)
--       ]

extractSurface :: V3 Int -> V.Vector Block -> IO (Ptr CUChar, Int)
extractSurface (V3 x y z) chunk = do
  lenPtr <- new 0 :: IO (Ptr CInt)
  ptr <- SV.unsafeWith (SV.convert (V.map (fromIntegral . fromEnum) chunk)) 
           $ c_extractSurface lenPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
  len <- peek lenPtr
  let res = (ptr, fromIntegral len)
  res `deepseq` return res

foreign import ccall "extractSurface" c_extractSurface 
  :: Ptr CInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO (Ptr CUChar)

renderChunk :: V3 Int -> UniformLocation -> StateT Chunk IO ()
renderChunk pos modelLoc = do
  isChanged <- use chunkChanged
  when isChanged (updateChunk pos)
  n <- use chunkElements
  when (n > 0) $ do
    use chunkVbo >>= (bindBuffer ArrayBuffer $=) . Just
    liftIO $ do
      model <- toGLmatrix $ identity & translation +~
        (fromIntegral chunkSize *^ fmap fromIntegral pos)
      uniform modelLoc $= model

      vertexAttribPointer (AttribLocation 0) $=
        (KeepIntegral, VertexArrayDescriptor 4 Byte 0 (intPtrToPtr 0))
      vertexAttribArray (AttribLocation 0) $= Enabled

      drawArrays Triangles 0 $ fromIntegral n

