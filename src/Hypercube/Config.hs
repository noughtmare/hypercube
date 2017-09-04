{-|
Module      : Hypercube.Config
Description : Configuration data of hypercube
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This file contains all configuration variables like @chunkSize@ and @renderDistance@.

TODO: Eventually we want this file to be dynamically loaded at startup.
-}

module Hypercube.Config where

import Hypercube.Types
import Linear

-- The size of a chunk. For example a chunkSize of 16 means that each chunk is a 16x16x16 cube of blocks.
chunkSize :: Int
chunkSize = 16

-- The number of chunks that gets loaded in any given direction.
-- for example a render distance of 4 would render a 4x4x4 cube of chunks around the user.
renderDistance :: Int
renderDistance = 1

-- The function that generates the landscape
generatingF :: V3 Int -> Block
generatingF (V3 x y z)
  | fromIntegral y < (8 :: Double) * sin ((fromIntegral x / 16) * pi) * sin ((fromIntegral z / 16) * pi) = Stone
  | otherwise = Air

