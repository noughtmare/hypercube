{-|
Module      : Hypercube.Chunk.Faces
Description : Raw face data
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains all the raw data for each of the faces of a block.
-}

module Hypercube.Chunk.Faces where

import Linear
import Data.Word (Word8)
import Data.Vector

-- The format of a vertex is: V4 x y z w, where w is the block type. At the moment there's only the grass block type so this last variable is not really used, but in the future the w variable has to be set somewhere else.

northFace, southFace, eastFace, westFace, topFace, bottomFace :: Vector (V4 Word8)

northFace = fromList
  [ (V4 1 1 1 0) -- top    right
  , (V4 0 1 1 0) -- top    left
  , (V4 0 0 1 0) -- bottom left
  , (V4 0 0 1 0) -- bottom left
  , (V4 1 0 1 0) -- bottom right
  , (V4 1 1 1 0) -- top    right
  ]

southFace = fromList
  [ (V4 0 0 0 0) -- bottom left
  , (V4 0 1 0 0) -- top    left
  , (V4 1 1 0 0) -- top    right
  , (V4 1 1 0 0) -- top    right
  , (V4 1 0 0 0) -- bottom right
  , (V4 0 0 0 0) -- bottom left
  ]

eastFace = fromList
  [ (V4 1 1 0 0) -- top    right
  , (V4 1 1 1 0) -- top    left
  , (V4 1 0 1 0) -- bottom left
  , (V4 1 0 1 0) -- bottom left
  , (V4 1 0 0 0) -- bottom right
  , (V4 1 1 0 0) -- top    right
  ]

westFace = fromList
  [ (V4 0 0 1 0) -- bottom left
  , (V4 0 1 1 0) -- top    left
  , (V4 0 1 0 0) -- top    right
  , (V4 0 1 0 0) -- top    right
  , (V4 0 0 0 0) -- bottom right
  , (V4 0 0 1 0) -- bottom left
  ]

topFace = fromList
  [ (V4 0 1 0 (-16)) -- bottom left
  , (V4 0 1 1 (-16)) -- top    left
  , (V4 1 1 1 (-16)) -- top    right
  , (V4 1 1 1 (-16)) -- top    right
  , (V4 1 1 0 (-16)) -- bottom right
  , (V4 0 1 0 (-16)) -- bottom left
  ]

bottomFace = fromList
  [ (V4 1 0 1 (-16)) -- top    right
  , (V4 0 0 1 (-16)) -- top    left
  , (V4 0 0 0 (-16)) -- bottom left
  , (V4 0 0 0 (-16)) -- bottom left
  , (V4 1 0 0 (-16)) -- bottom right
  , (V4 1 0 1 (-16)) -- top    right
  ]

