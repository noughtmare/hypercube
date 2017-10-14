module Hypercube.Error (
    printErrors
) where

import Control.Monad
import qualified Graphics.GL as Raw

getErrors :: IO [Raw.GLuint]
getErrors = do
    error <- Raw.glGetError
    if error == Raw.GL_NO_ERROR
        then return []
        else (error:) <$> getErrors

showError :: Raw.GLuint-> String
showError error = case error of
    Raw.GL_INVALID_ENUM -> "GL_INVALID_ENUM"
    Raw.GL_INVALID_VALUE -> "GL_INVALID_VALUE"
    Raw.GL_INVALID_OPERATION -> "GL_INVALID_OPERATION"
    Raw.GL_INVALID_FRAMEBUFFER_OPERATION -> "GL_INVALID_FRAMEBUFFER_OPERATION"
    Raw.GL_OUT_OF_MEMORY -> "GL_OUT_OF_MEMORY"
    Raw.GL_STACK_UNDERFLOW -> "GL_STACK_UNDERFLOW"
    Raw.GL_STACK_OVERFLOW -> "GL_STACK_OVERFLOW"
    x -> "GL Error " ++ show x

printErrors :: String -> IO ()
printErrors prefix = do
    es <- map showError <$> getErrors
    unless (null es) $
        error (prefix ++ ": " ++ show es)
