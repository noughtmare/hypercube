{-# LANGUAGE CPP, DefaultSignatures, FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, DataKinds, TypeOperators #-}
-- |Support for writing "Linear" types to uniform locations in
-- shader programs.
module Graphics.GLUtil.Linear (AsUniform(..)) where
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Graphics.Rendering.OpenGL
import Graphics.GL.Core31
import Linear
import Unsafe.Coerce (unsafeCoerce)

-- | A type class for things we can write to uniform locations in
-- shader programs. We can provide instances of this class for types
-- from "Linear" without introducing orphan instances.
class AsUniform t where
  asUniform :: t -> UniformLocation -> IO ()
  default asUniform :: Uniform t => t -> UniformLocation -> IO ()
  asUniform x loc = uniform loc $= x

getUL :: UniformLocation -> GLint
getUL = unsafeCoerce

castVecComponent :: Ptr (t a) -> Ptr a
castVecComponent = castPtr

castMatComponent :: Ptr (t (f a)) -> Ptr a
castMatComponent = castPtr

instance AsUniform GLint where
  x `asUniform` loc = with x $ glUniform1iv (getUL loc) 1

instance AsUniform GLuint where
  x `asUniform` loc = with x $ glUniform1uiv (getUL loc) 1

instance AsUniform GLfloat where
  x `asUniform` loc = with x $ glUniform1fv (getUL loc) 1

instance AsUniform TextureUnit where
instance UniformComponent a => AsUniform (Index1 a) where
instance UniformComponent a => AsUniform (Color4 a) where
instance UniformComponent a => AsUniform (Color3 a) where
instance UniformComponent a => AsUniform (FogCoord1 a) where
instance UniformComponent a => AsUniform (Normal3 a) where
instance UniformComponent a => AsUniform (TexCoord4 a) where
instance UniformComponent a => AsUniform (TexCoord3 a) where
instance UniformComponent a => AsUniform (TexCoord2 a) where
instance UniformComponent a => AsUniform (TexCoord1 a) where
instance UniformComponent a => AsUniform (Vertex4 a) where
instance UniformComponent a => AsUniform (Vertex3 a) where
instance UniformComponent a => AsUniform (Vertex2 a) where

instance AsUniform (V1 GLint  ) where {v `asUniform` loc = with v $ glUniform1iv  (getUL loc) 1 . castVecComponent}
instance AsUniform (V1 GLuint ) where {v `asUniform` loc = with v $ glUniform1uiv (getUL loc) 1 . castVecComponent}
instance AsUniform (V1 GLfloat) where {v `asUniform` loc = with v $ glUniform1fv  (getUL loc) 1 . castVecComponent}

instance AsUniform (V2 GLint  ) where {v `asUniform` loc = with v $ glUniform2iv  (getUL loc) 1 . castVecComponent}
instance AsUniform (V2 GLuint ) where {v `asUniform` loc = with v $ glUniform2uiv (getUL loc) 1 . castVecComponent}
instance AsUniform (V2 GLfloat) where {v `asUniform` loc = with v $ glUniform2fv  (getUL loc) 1 . castVecComponent}

instance AsUniform (V3 GLint  ) where {v `asUniform` loc = with v $ glUniform3iv  (getUL loc) 1 . castVecComponent}
instance AsUniform (V3 GLuint ) where {v `asUniform` loc = with v $ glUniform3uiv (getUL loc) 1 . castVecComponent}
instance AsUniform (V3 GLfloat) where {v `asUniform` loc = with v $ glUniform3fv  (getUL loc) 1 . castVecComponent}

instance AsUniform (V4 GLint  ) where {v `asUniform` loc = with v $ glUniform4iv  (getUL loc) 1 . castVecComponent}
instance AsUniform (V4 GLuint ) where {v `asUniform` loc = with v $ glUniform4uiv (getUL loc) 1 . castVecComponent}
instance AsUniform (V4 GLfloat) where {v `asUniform` loc = with v $ glUniform4fv  (getUL loc) 1 . castVecComponent}

instance AsUniform (M22 GLfloat) where
  m `asUniform` loc = with m
                    $ glUniformMatrix2fv (getUL loc) 1 1 . castMatComponent

instance AsUniform (M33 GLfloat) where
  m `asUniform` loc = with m
                    $ glUniformMatrix3fv (getUL loc) 1 1 . castMatComponent

instance AsUniform (M44 GLfloat) where
  m `asUniform` loc = with m
                    $ glUniformMatrix4fv (getUL loc) 1 1 . castMatComponent

-- Support lists of vectors as uniform arrays of vectors.

instance AsUniform [V1 GLint  ] where {l `asUniform` loc = withArray l $ glUniform1iv  (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V1 GLuint ] where {l `asUniform` loc = withArray l $ glUniform1uiv (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V1 GLfloat] where {l `asUniform` loc = withArray l $ glUniform1fv  (getUL loc) (fromIntegral $ length l) . castVecComponent}

instance AsUniform [V2 GLint  ] where {l `asUniform` loc = withArray l $ glUniform2iv  (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V2 GLuint ] where {l `asUniform` loc = withArray l $ glUniform2uiv (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V2 GLfloat] where {l `asUniform` loc = withArray l $ glUniform2fv  (getUL loc) (fromIntegral $ length l) . castVecComponent}

instance AsUniform [V3 GLint  ] where {l `asUniform` loc = withArray l $ glUniform3iv  (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V3 GLuint ] where {l `asUniform` loc = withArray l $ glUniform3uiv (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V3 GLfloat] where {l `asUniform` loc = withArray l $ glUniform3fv  (getUL loc) (fromIntegral $ length l) . castVecComponent}

instance AsUniform [V4 GLint  ] where {l `asUniform` loc = withArray l $ glUniform4iv  (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V4 GLuint ] where {l `asUniform` loc = withArray l $ glUniform4uiv (getUL loc) (fromIntegral $ length l) . castVecComponent}
instance AsUniform [V4 GLfloat] where {l `asUniform` loc = withArray l $ glUniform4fv  (getUL loc) (fromIntegral $ length l) . castVecComponent}
