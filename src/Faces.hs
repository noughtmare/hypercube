module Faces where

import Linear

northFace, southFace, eastFace, westFace, topFace, bottomFace :: [(V3 Float, V2 Float)]

northFace = 
  [ (V3 1 1 1, V2 0 0) -- top    right
  , (V3 0 1 1, V2 1 0) -- top    left
  , (V3 0 0 1, V2 1 1) -- bottom left
  , (V3 0 0 1, V2 1 1) -- bottom left
  , (V3 1 0 1, V2 0 1) -- bottom right
  , (V3 1 1 1, V2 0 0) -- top    right
  ]

southFace = 
  [ (V3 0 0 0, V2 1 1) -- bottom left
  , (V3 0 1 0, V2 1 0) -- top    left
  , (V3 1 1 0, V2 0 0) -- top    right
  , (V3 1 1 0, V2 0 0) -- top    right
  , (V3 1 0 0, V2 0 1) -- bottom right
  , (V3 0 0 0, V2 1 1) -- bottom left
  ]

eastFace =
  [ (V3 1 1 0, V2 0 0) -- top    right
  , (V3 1 1 1, V2 1 0) -- top    left
  , (V3 1 0 1, V2 1 1) -- bottom left
  , (V3 1 0 1, V2 1 1) -- bottom left
  , (V3 1 0 0, V2 0 1) -- bottom right
  , (V3 1 1 0, V2 0 0) -- top    right
  ]
  
westFace = 
  [ (V3 0 0 1, V2 1 1) -- bottom left
  , (V3 0 1 1, V2 1 0) -- top    left
  , (V3 0 1 0, V2 0 0) -- top    right
  , (V3 0 1 0, V2 0 0) -- top    right
  , (V3 0 0 0, V2 0 1) -- bottom right
  , (V3 0 0 1, V2 1 1) -- bottom left
  ]

topFace =
  [ (V3 0 1 0, V2 1 1) -- bottom left
  , (V3 0 1 1, V2 1 0) -- top    left
  , (V3 1 1 1, V2 0 0) -- top    right
  , (V3 1 1 1, V2 0 0) -- top    right
  , (V3 1 1 0, V2 0 1) -- bottom right
  , (V3 0 1 0, V2 1 1) -- bottom left
  ]

bottomFace =
  [ (V3 1 0 1, V2 0 0) -- top    right
  , (V3 0 0 1, V2 1 0) -- top    left
  , (V3 0 0 0, V2 1 1) -- bottom left
  , (V3 0 0 0, V2 1 1) -- bottom left
  , (V3 1 0 0, V2 0 1) -- bottom right
  , (V3 1 0 1, V2 0 0) -- top    right
  ]

