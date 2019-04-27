{-# LANGUAGE RecordWildCards #-}

-- | A Flappy Bird clone. Click to flap.
-- Avoid the grey obstacles and don't touch the lava.
module Main where

import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (isJust)
import           Debug.Trace          (traceShow)
import           Text.Printf          (printf)

import           Linear.V2            (V2 (V2))

import           System.Directory     (getCurrentDirectory)
import           System.FilePath      ((</>))
import qualified System.Random        as Rand

import           Helm
import qualified Helm.Cmd             as Cmd
import           Helm.Color
import           Helm.Engine.SDL      (SDLEngine)
import qualified Helm.Engine.SDL      as SDL
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard        as Keyboard
import qualified Helm.Mouse           as Mouse
import qualified Helm.Sub             as Sub
import           Helm.Time            (Time)
import qualified Helm.Time            as Time

import           Lib.Drawing

-- | Represents the game actions for our game.
data Action
  = DoNothing -- ^ Do nothing.
  | Animate Double -- ^ Animate the player with a dt.
  | KeyDown String
  | KeyUp String
  | Restart -- ^ Restart the game after dying.
  | SetupObstacles Rand.StdGen -- ^ Setup the obstacles using an RNG.

-- | Represents the status of the player (i.e. where they're at).
data PlayerStatus
  = Playing -- ^ The player is playing the game.
  | Waiting -- ^ The player is waiting and needs to click to start the game.
  | Dead -- ^ The player is dead and needs to hit space to get to the waiting state.
  deriving (Eq, Ord, Show)

data Enemy = Enemy
  { position :: V2 Double
  }

-- | Represents the game state of the game.
data Model = Model
  { playerPos     :: V2 Double
  , playerVel     :: V2 Double
  , playerStatus  :: PlayerStatus
  , keyboardState :: Map.Map String Bool
  , timeScore     :: Time
  , timeSpeed     :: Double
  , enemies       :: [Enemy]
  }

initial :: (Model, Cmd SDLEngine Action)
initial =
  ( Model
      { playerPos = V2 0 0
      , playerVel = V2 0 0
      , playerStatus = Waiting
      , timeScore = 0
      , timeSpeed = 1
      , keyboardState =
          Map.fromList
            [("up", False), ("right", False), ("down", False), ("left", False)]
      , enemies = []
      }
  , Cmd.none)

-- | The gravity acceleration for the flapper.
-- Note that the Y component is positive as the downwards
-- direction in our view is the positive end of the Y-axis.
-- The origin (0, 0) is the center of the screen.
speed :: Double
speed = 0.3

windowDims :: V2 Int
windowDims = V2 800 600

flapperDims :: V2 Double
flapperDims = V2 50 50

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model {..} (Animate dt) =
  (model {playerPos = (playerPos + (vel * (V2 dt dt)))}, Cmd.none)
  where
    vel =
      V2 speed speed *
      V2
        (if keyboardState Map.! "right"
           then 1
           else if keyboardState Map.! "left"
                  then -1
                  else 0)
        (if keyboardState Map.! "down"
           then 1
           else if keyboardState Map.! "up"
                  then -1
                  else 0)
-- | The player has pressed space while on the death screen.
-- Restart the game.
update model@Model {..} Restart =
  if playerStatus /= Dead
    then (model, Cmd.none)
    else ( model
             { playerStatus = Waiting
             , playerPos = V2 0 0
             , playerVel = V2 0 0
             , timeScore = 0
             }
    -- Trigger a regeneration of obstacles
         , Cmd.none)
update model@Model {..} (KeyUp key) =
  (model {keyboardState = Map.insert key False keyboardState}, Cmd.none)
update model@Model {..} (KeyDown key) =
  (model {keyboardState = Map.insert key True keyboardState}, Cmd.none)
-- | Do nothing.
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions =
  Sub.batch
    [ Keyboard.downs $ \key ->
        (case key of
           Keyboard.RightKey -> KeyDown "right"
           Keyboard.LeftKey  -> KeyDown "left"
           Keyboard.UpKey    -> KeyDown "up"
           Keyboard.DownKey  -> KeyDown "down"
           _                 -> DoNothing)
    , Keyboard.ups $ \key ->
        (case key of
           Keyboard.RightKey -> KeyUp "right"
           Keyboard.LeftKey  -> KeyUp "left"
           Keyboard.UpKey    -> KeyUp "up"
           Keyboard.DownKey  -> KeyUp "down"
           _                 -> DoNothing)
    , Time.fps 60 Animate
    ]

-- | Turn some second value into a pretty pluralized form (for UI).
secondsText :: Time -> String
secondsText t =
  show s ++
  if s /= 1
    then " seconds"
    else " second"
  where
    s = round $ Time.inSeconds t

enemy :: V2 Double -> Form SDLEngine
enemy pos = move pos $ text $ Text.height 50 $ Text.toText ("Enemy")

-- | The overlay displayed when the player is dead.
deadOverlay :: Color -> Model -> Form SDLEngine
deadOverlay color Model {..} =
  group
    [ move (V2 0 (-25)) $
      text $
      Text.height 30 $ Text.color color $ Text.toText "Oops, you're dead."
    , move (V2 0 25) $
      text $ Text.height 12 $ Text.color color $ Text.toText score
    , move (V2 0 50) $
      text $
      Text.height 12 $ Text.color color $ Text.toText "Press space to restart"
    ]
  where
    score = "You lasted " ++ secondsText timeScore

-- | The overlay displayed when the player is waiting to play.
waitingOverlay :: Color -> Form SDLEngine
waitingOverlay color =
  group
    [ move (V2 0 (-75)) $
      text $ Text.height 30 $ Text.color color $ Text.toText "Ready?"
    , move (V2 0 75) $
      text $ Text.height 12 $ Text.color color $ Text.toText "Click to flap"
    ]

-- | The overlay when playing the game (i.e. HUD).
playingOverlay :: Color -> Model -> Form SDLEngine
playingOverlay color Model {..} =
  group
    [ move (V2 0 (-h / 2 + 25)) $
      text $ Text.height 12 $ Text.color color $ Text.toText status
    ]
  where
    status = secondsText timeScore ++ " | " ++ printf "%.2fx speed" timeSpeed
    V2 _ h = fromIntegral <$> windowDims

view :: Map.Map String (Image SDLEngine) -> Model -> Graphics SDLEngine
view assets model@Model {..} =
  Graphics2D $
  collage
    [ moveTerrain playerPos $ terrainSprite assets
    , move (V2 (w / 2) (h / 2)) (playerSprite assets)
    ]
  where
    dims@(V2 w h) = fromIntegral <$> windowDims
    overlayColor = rgb 1 1 1
    overlay Waiting _     = waitingOverlay overlayColor
    overlay Dead model    = deadOverlay overlayColor model
    overlay Playing model = playingOverlay overlayColor model

main :: IO ()
main = do
  engine <-
    SDL.startupWith $
    SDL.defaultConfig
      {SDL.windowIsResizable = False, SDL.windowDimensions = windowDims}
  imageDir <- (</> "assets") <$> getCurrentDirectory
  let assetList = [("Soul.png", "soul"), ("grass.png", "grass")]
      loadAssets' [] game loaded = game loaded
      loadAssets' ((file, id):files) game loaded = do
        SDL.withImage engine (imageDir </> file) $ \image ->
          loadAssets' files game (Map.insert id image loaded)
      loadAssets files game = loadAssets' files game Map.empty
  loadAssets assetList $ \allAssets ->
    run
      engine
      GameConfig
        { initialFn = initial
        , updateFn = update
        , subscriptionsFn = subscriptions
        , viewFn = view allAssets
        }
