module Lib.Drawing
  ( playerSprite
  , terrainSprite
  , moveTerrain
  ) where

import           Data.Fixed
import qualified Data.Map        as Map
import           Helm
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D
import           Linear.V2       (V2 (V2))

playerDims :: V2 Double
playerDims = V2 50 50

grassDims :: V2 Double
grassDims = V2 100 100

tile :: Form e -> Collage e
tile form =
  collage $
  concat
    (map
       (\i -> (map (\j -> (move (V2 (i * 100) (j * 100)) form))) [0 .. 10])
       [0 .. 10])

playerSprite :: Map.Map String (Image SDLEngine) -> Form SDLEngine
playerSprite assets = image playerDims (assets Map.! "soul")

terrainSprite :: Map.Map String (Image SDLEngine) -> Form SDLEngine
terrainSprite assets = toForm $ tile $ image grassDims $ assets Map.! "grass"

moveTerrain :: V2 Double -> Form SDLEngine -> Form SDLEngine
moveTerrain playerPos terrain =
  move ((\x -> (mod' (-x) grassX) - grassX) <$> playerPos) terrain
  where
    V2 x y = playerPos
    V2 grassX grassY = grassDims
