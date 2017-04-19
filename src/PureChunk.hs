{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module PureChunk
  ( newChunk
  , getBlock
  , setBlock
  -- , updateChunk
  , renderChunk
  ) where

import qualified Debug.Trace as D

import Util
import Types
import Config (chunkSize, generatingF)
import Faces

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Storable as SV
import Data.Vector.Mutable (IOVector)
import Graphics.Rendering.OpenGL hiding (get)
import Linear
import Control.Monad
import Data.IORef
import Control.Lens ((+~), (-~), (&), (.~), (^.))
import Foreign.Storable (sizeOf)
import Foreign.Ptr (intPtrToPtr)
import Data.Foldable (toList)
import qualified Data.Vector as V (generate, thaw)
import Control.Concurrent
import qualified Data.Map as M
import Data.Maybe
import Control.Lens
import Data.Word (Word8)
import Control.Monad.State.Strict
import Control.DeepSeq
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.Thyme.Clock
import Data.AffineSpace ((.-.))

toPos :: V3 Int -> Int
toPos (V3 x y z) = x + chunkSize * y + (chunkSize ^ 2) * z
{-# INLINE toPos #-}

fromPos :: Int -> V3 Int
fromPos n =
    let (n',x) = n `quotRem` chunkSize
        (z,y)  = n' `quotRem` chunkSize
    in V3 x y z
{-# INLINE fromPos #-}

newChunk :: V3 Int -> TChan (V.Vector (V4 Word8), BufferObject, MVar Int)-> IO PureChunk
newChunk v chan = do
  vbo <- genObjectName
  var <- newEmptyMVar
  return $ PureChunk
    (V.generate (chunkSize ^ 3) (generatingF . (+ chunkSize *^ v) . fromPos))
    vbo
    0
    True
    v
    False
    chan
    var

getBlock :: PureChunk -> V3 Int -> Block
getBlock c v = (c ^. pchunkBlk) V.! toPos v

setBlock :: PureChunk -> V3 Int -> Block -> PureChunk
setBlock c v b = c & pchunkBlk %~ V.modify (\x -> M.write x (toPos v) b)

updateChunk :: StateT PureChunk IO ()
updateChunk = do
  isChanging <- use pchunkChanging
  var <- use pchunkIsLoaded
  when (not isChanging) $ do
    pchunkChanging .= True
    blk  <- use pchunkBlk
    chan <- use pchunkChan
    vbo  <- use pchunkVbo
    pos <- use pchunkPos
    lift $ void $ forkIO $ do
      let
        vertices = force $ do
          (p,_) <- V.filter ((/= Air) . snd) $ V.indexed blk
          let v = fromPos p
          (v',face) <- V.fromList $ zip
                         [ v & dir +~ mag | dir <- [_z,_y,_x], mag <- [1,-1]]
                         [ northFace & traverse . _w +~ 1
                         , southFace & traverse . _w +~ 1
                         , topFace
                         , bottomFace
                         , eastFace & traverse . _w +~ 1
                         , westFace & traverse . _w +~ 1
                         ]
          guard $ all (\x -> x >= 0 && x < chunkSize) v' && blk V.! (toPos v') == Air || generatingF (v' + chunkSize *^ pos) == Air
          face & traverse +~ (0 & _xyz .~ fmap fromIntegral v)
      vertices `seq` atomically $ writeTChan chan (vertices,vbo,var)

  mayTemp <- lift $ tryTakeMVar var
  case mayTemp of
    Nothing -> return ()
    Just l -> do
      pchunkChanged .= False
      pchunkChanging .= False
      pchunkElements .= l

renderChunk :: PureChunk -> UniformLocation -> IO PureChunk
renderChunk c modelLoc = flip execStateT c $ do
  isChanged <- use pchunkChanged
  when isChanged $ updateChunk
  n <- use pchunkElements
  when (n /= 0) $ do
    use pchunkVbo >>= (bindBuffer ArrayBuffer $=) . Just
    pos <- use pchunkPos

    lift $ do
      model <- toGLmatrix $ identity & translation +~
        (fromIntegral chunkSize *^ (fmap fromIntegral pos))
      uniform modelLoc $= model

      vertexAttribPointer (AttribLocation 0) $=
        (ToFloat, VertexArrayDescriptor 4 Byte 0 (intPtrToPtr 0))
      vertexAttribArray (AttribLocation 0) $= Enabled

      drawArrays Triangles 0 $ fromIntegral n
