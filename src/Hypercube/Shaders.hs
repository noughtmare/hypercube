{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Hypercube.Shaders
Description : The shaders
Copyright   : (c) Jaro Reinders, 2017
License     : GPL-3
Maintainer  : noughtmare@openmailbox.org

This module contains the shaders as bytestrings and a function that produces an OpenGL @Program@ that uses those shaders.

The shaders are embedded to make sure that they are present when running the game.
-}
module Hypercube.Shaders (shaders) where
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as B
import System.Exit
import Control.Monad
import Data.FileEmbed

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
vector = $(embedFile "vertex.glsl")

fragment :: B.ByteString
fragment = $(embedFile "fragment.glsl")
