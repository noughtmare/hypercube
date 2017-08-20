{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Hypercube.Shaders
Description : The shaders
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains the shaders as strings and a function that produces an OpenGL @Program@ that uses those shaders.

The shaders are inlined to make sure that they are present when running the game.
-}
module Hypercube.Shaders (shaders) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as B
import System.Exit
import Control.Monad

shaders :: IO GL.Program
shaders = do
  v <- GL.createShader GL.VertexShader
  GL.shaderSourceBS v GL.$= vector
  GL.compileShader v
  vs <- GL.get (GL.compileStatus v)
  unless vs $ do
    print =<< GL.get (GL.shaderInfoLog v)
    exitFailure

  f <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS f GL.$= fragment
  GL.compileShader f
  fs <- GL.get (GL.compileStatus f)
  unless fs $ do
    putStrLn =<< GL.get (GL.shaderInfoLog f)
    exitFailure

  p <- GL.createProgram
  GL.attachShader p v
  GL.attachShader p f
  GL.linkProgram p
  return p

vector :: B.ByteString
vector = 
  "#version 330 core\n\

  \layout (location = 0) in vec4 position;\

  \uniform mat4 model;\
  \uniform mat4 view;\
  \uniform mat4 projection;\

  \out vec4 pos;\

  \void main() {\
  \  gl_Position = projection * view * model * vec4(position.xyz, 1);\
  \  pos = position;\
  \}\
  \"

fragment :: B.ByteString
fragment =
  "#version 330 core\n\

  \in vec4 pos;\

  \out vec4 color;\

  \uniform sampler2D tex;\

  \void main() {\
  \  if (pos.w < 0)\
  \    color = texture(tex, vec2((fract(pos.x) + pos.w) / 16.0, 1 - pos.z));\
  \  else\
  \    color = texture(tex, vec2((fract(pos.x + pos.z) + pos.w) / 16.0, 1 - pos.y));\
  \}\
  \"
