{-# LANGUAGE TemplateHaskell #-}
module Types where

import Linear
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Map as M
import Data.IORef
import Control.Arrow
import Control.Concurrent.MVar
import Data.Word (Word8)
import Control.Concurrent.STM.TChan

data Camera
  = Camera
  { _camPos :: !(V3 Float)
  , _jaw :: !Float
  , _pitch :: !Float
  , _speed :: !Float
  , _sensitivity :: !Float
  } deriving (Show)

makeLensesFor [("_camPos","camPos"),("_jaw","jaw"),("_speed","speed"),("_sensitivity","sensitivity")] ''Camera

-- | "Advanced" lensing (we check bounds inside the setter)
pitch :: Lens' Camera Float
pitch = lens _pitch setter
  where
    setter cam x
      | abs x > pi/2 - epsilon = cam {_pitch = (signum x) * (pi / 2 - epsilon)}
      | otherwise              = cam {_pitch = x}
    epsilon = 0.005

gaze :: Getter Camera (V3 Float)
gaze = to $ gaze' . (view jaw &&& view pitch)
  where
    gaze' (j,p)
      = rotate (axisAngle (V3 0 1 0) j)
      $ rotate (axisAngle (V3 1 0 0) p)
      $ V3 0 0 (-1)

data Game
  = Game
  { _cam       :: !Camera
  , _lastFrame :: !Float
  , _cursorPos :: !(V2 Float)
  , _world     :: !(IORef (M.Map (V3 Int) PureChunk))
  }

instance Show Chunk where
  show _ = "chunk"

data Block
  = Air
  | Stone
  deriving (Show, Eq)

data Chunk
  = Chunk
  { chunkBlk      :: !(IOVector Block)
  , chunkVbo      :: !GL.BufferObject
  , chunkElements :: !(IORef Int)
  , chunkChanged  :: !(IORef Bool)
  , chunkPos      :: !(V3 Int)
  , chunkWorld    :: !(IORef (M.Map (V3 Int) Chunk))
  }

data PureChunk
  = PureChunk
  { _pchunkBlk      :: !(V.Vector Block)
  , _pchunkVbo      :: !GL.BufferObject
  , _pchunkElements :: !Int
  , _pchunkChanged  :: !Bool
  , _pchunkPos      :: !(V3 Int)
  , _pchunkChanging :: !Bool
  , _pchunkChan     :: !(TChan (V.Vector (V4 Word8), GL.BufferObject, MVar Int))
  , _pchunkIsLoaded :: !(MVar Int)
  }

makeLenses ''PureChunk
makeLenses ''Game
