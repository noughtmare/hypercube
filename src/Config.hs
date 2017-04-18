module Config where

import Types
import Linear

chunkSize :: Int
chunkSize = 16

renderDistance :: Int
renderDistance = 7

generatingF :: V3 Int -> Block
generatingF (V3 x y z)
  | fromIntegral y < 8 * sin (fromIntegral x / 16 * pi) * cos (fromIntegral z / 16 * pi) = Stone
  | otherwise = Air

