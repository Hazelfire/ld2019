module Lib.Drawing
  ( playerSprite
  ) where

import qualified Data.Map        as Map
import           Helm
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D
import           Linear.V2       (V2 (V2))

playerDims :: V2 Double
playerDims = V2 50 50

playerSprite :: Map.Map String (Image SDLEngine) -> Form SDLEngine
playerSprite assets = image playerDims (assets Map.! "soul")
