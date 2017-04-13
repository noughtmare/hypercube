module Config where

import Types
import Linear

chunkSize :: Int
chunkSize = 16

renderDistance :: Int
renderDistance = 5

generatingF :: V3 Int -> Block
generatingF (V3 x y z)
  | y < ceiling (10 * sin (fromIntegral x / 10) * cos (fromIntegral z / 10)) = Stone
  | otherwise = Air

