{-# LANGUAGE LambdaCase #-}

{-|
Module      : Hypercube.ChunkManager
Description : Manages the loading of Chunks
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains a background process which manages the loading and unloading of chunks.
-}

module Hypercube.ChunkManager where

--import Data.MVar

{-
-- NOTE: we refer to chunks by ther position (V3 Int)
data Request
  | GenChunk   (V3 Int)
  | LoadChunk  (V3 Int)
  | GetBuffers (MVar [ChunkBuffer])

startChunkMan :: IO (MVar Request)
startChunkMan = do
  req <- newEmptyMVar
  forkIO $ forever $ do
    takeMVar request >>= \case
      GenChunk pos -> do
        undefined
      LoadChunk pos -> do
        undefined
      GetBuffers response -> do
        undefined
  return req
-}
