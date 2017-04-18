module Faces where

import Linear
import Data.Word (Word8)
import Data.Vector

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

