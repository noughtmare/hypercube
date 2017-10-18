{-# LANGUAGE LambdaCase #-}
module Hypercube.Error (printErrors) where

import Control.Monad
import Graphics.GL

getErrors :: IO [GLuint]
getErrors = do
  err <- glGetError
  if err == GL_NO_ERROR
    then return []
    else (err:) <$> getErrors

showError :: GLuint-> String
showError = \case
  GL_INVALID_ENUM                  -> "GL_INVALID_ENUM"
  GL_INVALID_VALUE                 -> "GL_INVALID_VALUE"
  GL_INVALID_OPERATION             -> "GL_INVALID_OPERATION"
  GL_INVALID_FRAMEBUFFER_OPERATION -> "GL_INVALID_FRAMEBUFFER_OPERATION"
  GL_OUT_OF_MEMORY                 -> "GL_OUT_OF_MEMORY"
  GL_STACK_UNDERFLOW               -> "GL_STACK_UNDERFLOW"
  GL_STACK_OVERFLOW                -> "GL_STACK_OVERFLOW"
  x                                -> "GL Error " ++ show x

printErrors :: String -> IO ()
printErrors prefix = do
  es <- map showError <$> getErrors
  unless (null es) $ error (prefix ++ ": " ++ show es)
