{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Arrow
import Control.Concurrent.MVar
import Data.Word (Word8)
import Control.Concurrent.STM.TChan

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
  , _world     :: !(M.Map (V3 Int) Chunk)
  }

data Block
  = Air
  | Stone
  deriving (Show, Eq)

data Chunk
  = Chunk
  { _chunkBlk      :: !(V.Vector Block)
  , _chunkVbo      :: !GL.BufferObject
  , _chunkElements :: !Int
  , _chunkChanged  :: !Bool
  , _chunkPos      :: !(V3 Int)
  , _chunkChanging :: !Bool
  , _chunkChan     :: !(TChan (V.Vector (V4 Word8), GL.BufferObject, MVar Int))
  , _chunkIsLoaded :: !(MVar Int)
  }

makeLenses ''Chunk
makeLenses ''Game
