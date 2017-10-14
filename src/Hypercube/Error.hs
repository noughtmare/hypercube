module Hypercube.Error (
    printErrors
) where

import Control.Monad
import Graphics.GL

getErrors :: IO [GLuint]
getErrors = do
    error <- glGetError
    if error == GL_NO_ERROR
        then return []
        else (error:) <$> getErrors

showError :: GLuint-> String
showError error = case error of
    GL_INVALID_ENUM -> "GL_INVALID_ENUM"
    GL_INVALID_VALUE -> "GL_INVALID_VALUE"
    GL_INVALID_OPERATION -> "GL_INVALID_OPERATION"
    GL_INVALID_FRAMEBUFFER_OPERATION -> "GL_INVALID_FRAMEBUFFER_OPERATION"
    GL_OUT_OF_MEMORY -> "GL_OUT_OF_MEMORY"
    GL_STACK_UNDERFLOW -> "GL_STACK_UNDERFLOW"
    GL_STACK_OVERFLOW -> "GL_STACK_OVERFLOW"
    x -> "GL Error " ++ show x

printErrors :: String -> IO ()
printErrors prefix = do
    es <- map showError <$> getErrors
    unless (null es) $
        error (prefix ++ ": " ++ show es)
