module Faces where

import Linear

northFace, southFace, eastFace, westFace, topFace, bottomFace :: [(V3 Float, V2 Float, Float)]

northFace = 
  [ (V3 1 1 1, V2 0 0, 0.7) -- top    right
  , (V3 0 1 1, V2 1 0, 0.7) -- top    left
  , (V3 0 0 1, V2 1 1, 0.7) -- bottom left
  , (V3 0 0 1, V2 1 1, 0.7) -- bottom left
  , (V3 1 0 1, V2 0 1, 0.7) -- bottom right
  , (V3 1 1 1, V2 0 0, 0.7) -- top    right
  ]

southFace = 
  [ (V3 0 0 0, V2 1 1, 0.7) -- bottom left
  , (V3 0 1 0, V2 1 0, 0.7) -- top    left
  , (V3 1 1 0, V2 0 0, 0.7) -- top    right
  , (V3 1 1 0, V2 0 0, 0.7) -- top    right
  , (V3 1 0 0, V2 0 1, 0.7) -- bottom right
  , (V3 0 0 0, V2 1 1, 0.7) -- bottom left
  ]

eastFace =
  [ (V3 1 1 0, V2 0 0, 0.6) -- top    right
  , (V3 1 1 1, V2 1 0, 0.6) -- top    left
  , (V3 1 0 1, V2 1 1, 0.6) -- bottom left
  , (V3 1 0 1, V2 1 1, 0.6) -- bottom left
  , (V3 1 0 0, V2 0 1, 0.6) -- bottom right
  , (V3 1 1 0, V2 0 0, 0.6) -- top    right
  ]
  
westFace = 
  [ (V3 0 0 1, V2 1 1, 0.6) -- bottom left
  , (V3 0 1 1, V2 1 0, 0.6) -- top    left
  , (V3 0 1 0, V2 0 0, 0.6) -- top    right
  , (V3 0 1 0, V2 0 0, 0.6) -- top    right
  , (V3 0 0 0, V2 0 1, 0.6) -- bottom right
  , (V3 0 0 1, V2 1 1, 0.6) -- bottom left
  ]

topFace =
  [ (V3 0 1 0, V2 1 1, 0.8) -- bottom left
  , (V3 0 1 1, V2 1 0, 0.8) -- top    left
  , (V3 1 1 1, V2 0 0, 0.8) -- top    right
  , (V3 1 1 1, V2 0 0, 0.8) -- top    right
  , (V3 1 1 0, V2 0 1, 0.8) -- bottom right
  , (V3 0 1 0, V2 1 1, 0.8) -- bottom left
  ]

bottomFace =
  [ (V3 1 0 1, V2 0 0, 0.5) -- top    right
  , (V3 0 0 1, V2 1 0, 0.5) -- top    left
  , (V3 0 0 0, V2 1 1, 0.5) -- bottom left
  , (V3 0 0 0, V2 1 1, 0.5) -- bottom left
  , (V3 1 0 0, V2 0 1, 0.5) -- bottom right
  , (V3 1 0 1, V2 0 0, 0.5) -- top    right
  ]

