{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Hypercube.Types
Description : Avoiding circular dependencies
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains all the types in one place to avoid circular dependencies.
-}

module Hypercube.Types where

import Linear
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as M
import Control.Arrow
import Control.DeepSeq
import qualified Data.Vector.Storable as VS
import Data.Int
import Data.Vector.Storable (Storable)
import Foreign (Storable(..), castPtr)
import Data.Word (Word8)

data Camera
  = Camera
  { _camPos      :: !(V3 Float)
  , _jaw         :: !Float
  , _pitch       :: !Float
  , _speed       :: !Float
  , _sensitivity :: !Float
  } deriving (Show)

makeLensesFor
  [ ("_camPos","camPos")
  , ("_jaw","jaw")
  , ("_speed","speed")
  , ("_sensitivity","sensitivity")
  ] ''Camera

-- | "Advanced" lensing (we check bounds inside the setter)
pitch :: Lens' Camera Float
pitch = lens _pitch setter
  where
    setter cam x
      | abs x > pi/2 - epsilon = cam {_pitch = signum x * (pi / 2 - epsilon)}
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
  , _world     :: !(M.Map (V3 Int) Chunk)
  }

data Block
  = Air
  | Stone
  deriving (Show, Eq, Enum)

instance Storable Block where
  sizeOf _ = 1
  alignment _ = 1
  peek p = toEnum . fromIntegral <$> peek @Word8 (castPtr p)
  poke p = poke @Word8 (castPtr p) . fromIntegral . fromEnum

instance NFData Block where
  rnf Air = ()
  rnf Stone = ()

data Chunk
  = Chunk
  { _chunkBlk      :: !(VS.Vector Block)
  , _chunkVbo      :: GL.BufferObject -- ^ No need to keep it around.
  , _chunkVao      :: GL.VertexArrayObject
  , _chunkElements :: !Int
  , _chunkChanged  :: !Bool
  }

makeLenses ''Chunk
makeLenses ''Game

data ChunkResponse = ChunkResponse !(V3 Int) !Chunk !(VS.Vector (V4 Int8)) 