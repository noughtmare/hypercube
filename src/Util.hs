module Util where

import Graphics.Rendering.OpenGL
import Data.Foldable
import Linear
import Control.Monad

toGLmatrix :: M44 Float -> IO (GLmatrix Float)
toGLmatrix = newMatrix RowMajor . (toList >=> toList)

