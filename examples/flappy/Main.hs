{-# LANGUAGE RecordWildCards #-}
-- | A Flappy Bird clone. Click to flap.
-- Avoid the grey obstacles and don't touch the lava.
module Main where

import           Data.List (find)
import           Data.Maybe (isJust)
import           Debug.Trace (traceShow)
import           Text.Printf (printf)

import           Linear.V2 (V2(V2))
import qualified System.Random as Rand

import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import           Helm.Time (Time)

-- | Represents the game actions for our game.
data Action
  = DoNothing                   -- ^ Do nothing.
  | Animate Double              -- ^ Animate the player with a dt.
  | Flap                        -- ^ Flap the player.
  | Restart                     -- ^ Restart the game after dying.
  | SetupObstacles Rand.StdGen  -- ^ Setup the obstacles using an RNG.

-- | Represents the status of the player (i.e. where they're at).
data PlayerStatus
  = Playing  -- ^ The player is playing the game.
  | Waiting  -- ^ The player is waiting and needs to click to start the game.
  | Dead     -- ^ The player is dead and needs to hit space to get to the waiting state.
  deriving (Eq, Ord, Show)

-- | Represents an obstacle the flapper can hit.
data Obstacle
  = NoObstacle  -- ^ The obstacle exists, but it's just empty (not visible and uncollidable).
  | Obstacle    -- ^ An actually collidable obstacle.
    { obsTopLeft     :: V2 Double
    , obsBottomRight :: V2 Double
    } deriving (Eq, Ord, Show)

-- | Represents the game state of the game.
data Model = Model
  { flapperPos   :: V2 Double
  , flapperVel   :: V2 Double
  , playerStatus :: PlayerStatus
  , obstacles    :: [Obstacle]
  , timeScore    :: Time
  , timeSpeed    :: Double
  }

initial :: (Model, Cmd SDLEngine Action)
initial =
  ( Model
      { flapperPos   = V2 0 0
      , flapperVel   = V2 0 0
      , playerStatus = Waiting
      , obstacles    = []
      , timeScore    = 0
      , timeSpeed    = 1
      }
  , Cmd.execute Rand.newStdGen SetupObstacles
  )

-- | The gravity acceleration for the flapper.
-- Note that the Y component is positive as the downwards
-- direction in our view is the positive end of the Y-axis.
-- The origin (0, 0) is the center of the screen.
gravity :: V2 Double
gravity = V2 0 7

lavaHeight :: Double
lavaHeight = 65

windowDims :: V2 Int
windowDims = V2 800 600

scrollVel :: V2 Double
scrollVel = V2 4 0

obsWidth :: Double
obsWidth = 70

obsMargin :: Double
obsMargin = 50

obsOffset :: Double
obsOffset = (obsMargin + obsWidth) * 6

flapperDims :: V2 Double
flapperDims = V2 50 50

-- | Only the obstacles the player has seen/can see.
relevantObs :: Model -> [Obstacle]
relevantObs Model { .. } =
  take n obstacles

  where
    V2 x _ = flapperPos
    V2 w _ = fromIntegral <$> windowDims
    n = max 0 $ floor $ (x - obsOffset + w) / (obsMargin + obsWidth)

-- | Are any obstacles touching the flapper?
touchingObs :: Model -> Bool
touchingObs model@Model { .. } =
  isJust $ find f $ relevantObs model

  where
    V2 x y = flapperPos
    V2 w h = flapperDims

    -- The flapper pos is centered. Turn it into a box.
    ftx = x - w / 2
    fty = y - h / 2
    fbx = x + w / 2
    fby = y + h / 2

    -- Check if the flapper box and obs box intersect.
    -- If so, we're dead.
    f NoObstacle = False
    f Obstacle { .. } =
      max tx ftx < min bx fbx &&
      max ty fty < min by fby

      where
        V2 tx ty = obsTopLeft
        V2 bx by = obsBottomRight

-- | Is our flapper touching the lava at the bottom of the screen?
inLava :: Model -> Bool
inLava Model { .. } =
  y + fh  / 2 >= h / 2 - lavaHeight

  where
    V2 _ fh = flapperDims
    V2 x y = flapperPos
    V2 w h = fromIntegral <$> windowDims

-- | Should our flapper die? Only checks if they should -
-- DOES NOT transition the player status to dead.
shouldDie :: Model -> Bool
shouldDie model = inLava model || touchingObs model

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { .. } (Animate dt) =
  if playerStatus == Waiting then (model, Cmd.none)
  else
    ( model
      { flapperPos   = if y < -hh
                       then V2 x (-hh)
                       else pos
      , flapperVel   = vel
      , playerStatus = if dead
                       then Dead
                       else Playing
      , timeScore    = elapsed
      , timeSpeed    = speed
      }
    , Cmd.none
    )

  where
    -- | If the player is actually playing, increase the score.
    -- They might be at the death screen (which is also animated).
    elapsed = if dead
              then timeScore
              else timeScore + dt

    dt' = dt * 0.005
    gravity' = gravity * V2 dt' dt'

    -- Make the movement right faster as the player gets further across.
    speed = logBase 10 (10 + Time.inSeconds elapsed)
    vel = if dead
          then V2 0 1 * (flapperVel + gravity') -- No x-velocity while dead.
          else flapperVel + gravity'

    pos@(V2 x y) = flapperPos + vel + if dead
                                      then V2 0 0
                                      else scrollVel * V2 speed 0

    V2 _ h = fromIntegral <$> windowDims
    hh = h / 2
    dead = (playerStatus == Dead) || shouldDie model

-- | The player has clicked using their mouse.
-- | Process the "flap" of our flapper's wings.
update model@Model { .. } Flap =
  if playerStatus == Dead
  then (model, Cmd.none)
  else
    ( model
      { flapperVel   = V2 0 (-17)
      , playerStatus = Playing
      }
    , Cmd.none
    )

-- | The player has pressed space while on the death screen.
-- Restart the game.
update model@Model { .. } Restart =
  if playerStatus /= Dead
  then (model, Cmd.none)
  else
    ( model
      { playerStatus = Waiting
      , flapperPos   = V2 0 0
      , flapperVel   = V2 0 0
      , timeScore    = 0
      }

    -- Trigger a regeneration of obstacles
    , Cmd.execute Rand.newStdGen SetupObstacles
    )

-- | Initialize a list of all the obstacles in the game.
-- This works really nicely, as the list of obstacles
-- we produce is lazy and we can just keep generating
-- new obstacles out of until infinity - which is perfect,
-- as our player might just keep playing the game
-- for an eternity and beyond.
update model (SetupObstacles rng) =
  (model
   { obstacles = scanl generate NoObstacle $
                 zip [0..] $
                 Rand.randoms rng
   }
  , Cmd.none
  )

  where
    -- | Generate an obstacle based based off the last obstacle
    -- generated and some random input.
    generate
      :: Obstacle      -- | The last obstacle generated (or 'NoObstacle' if first).
      -> (Int, Double) -- | First element is the obstacle index, second is random input between [0,1).
      -> Obstacle      -- | The generated obstacle.
    generate last (i, n) =
      -- Randomly exclude obstacles, but don't do it for the first one.
      if i > 0 && (n < lb || n > hb)
      then NoObstacle
      else
        Obstacle
          { obsTopLeft = V2 x y
          , obsBottomRight = V2 (x + obsWidth) (y + height)
          }

      where
        lb = 0.2
        hb = 0.8

        -- We have to normalize the value here as we ignored < lb and > hb above.
        -- Let's get this back to (0, 1]
        n' = (n - lb) / (hb - lb)
        x = obsOffset + (obsWidth + obsMargin) * fromIntegral i
        V2 _ h = fromIntegral <$> windowDims
        h' = h - lavaHeight
        hh' = h / 2
        minHeight = 100
        maxHeight = 300
        (y, height) = calc last

        -- | Calc the obstacle height and
        calc NoObstacle =
          -- Generate a random y and random height. This can be anywhere on the screen as
          -- we're not next to an older obstacle.
          (-hh' + n' * h', height')

          where
            height' = minHeight + (maxHeight - minHeight) * n'

        calc Obstacle { .. } =
          -- We want the obstacle being generated to be in a similar position
          -- to the last one generated. So we adjust the last position randomly
          -- by a portion of the last generated obstacle's height.
          ( max (-hh') $ min hh' $ ty + (n' - 0.5) * maxHeight
          , height')

          where
            V2 tx ty = obsTopLeft
            V2 bx by = obsBottomRight
            lastHeight = by - ty
            height' = min maxHeight $ lastHeight + (n' - 0.5) * minHeight

-- | Do nothing.
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Mouse.clicks $ \_ _ -> Flap
  , Keyboard.presses $ \key -> (case key of
      Keyboard.SpaceKey -> Restart
      _                 -> DoNothing)
  , Time.fps 60 Animate
  ]

-- | Turn some second value into a pretty pluralized form (for UI).
secondsText :: Time -> String
secondsText t =
  show s ++ if s /= 1 then " seconds" else " second"

  where
    s = round $ Time.inSeconds t

-- | The overlay displayed when the player is dead.
deadOverlay :: Color -> Model -> Form SDLEngine
deadOverlay color Model { ..  } =
 group
   [ move (V2 0 (-25)) $ text $ Text.height 30 $
                                Text.color color $
                                Text.toText "Oops, you're dead."

   , move (V2 0 25) $ text $ Text.height 12 $
                             Text.color color $
                             Text.toText score

   , move (V2 0 50) $ text $ Text.height 12 $
                             Text.color color $
                             Text.toText "Press space to restart"
   ]

  where
    score = "You lasted " ++ secondsText timeScore

-- | The overlay displayed when the player is waiting to play.
waitingOverlay :: Color -> Form SDLEngine
waitingOverlay color =
  group
    [ move (V2 0 (-75)) $ text $ Text.height 30 $
                                 Text.color color $
                                 Text.toText "Ready?"

    , move (V2 0 75) $ text $ Text.height 12 $
                              Text.color color $
                              Text.toText "Click to flap"
    ]

-- | The overlay when playing the game (i.e. HUD).
playingOverlay :: Color -> Model -> Form SDLEngine
playingOverlay color Model { .. } =
  group
    [
      move (V2 0 (-h / 2 + 25)) $ text $ Text.height 12 $
                                         Text.color color $
                                         Text.toText status
    ]

  where
    status = secondsText timeScore ++ " | " ++ printf "%.2fx speed" timeSpeed
    V2 _ h = fromIntegral <$> windowDims

view :: Model -> Graphics SDLEngine
view model@Model { .. } = Graphics2D $
  center (V2 (w / 2) (h / 2)) $ collage
    [ backdrop
    , toForm $ center (V2 (-x) 0) $ collage
        [ move flapperPos flapper
        , group $ map structure $ relevantObs model
        ]

    , lava
    , overlay playerStatus model
    ]

  where
    dims@(V2 w h) = fromIntegral <$> windowDims
    V2 x y = flapperPos
    overlayColor = rgb 1 1 1
    overlay Waiting _ = waitingOverlay overlayColor
    overlay Dead model = deadOverlay overlayColor model
    overlay Playing model = playingOverlay overlayColor model
    flapper = filled (rgb 0.36 0.25 0.22) $ rect flapperDims
    backdrop = filled (rgb 0.13 0.13 0.13) $ rect dims
    lava = move (V2 0 (h / 2 - lavaHeight / 2)) $ filled (rgb 0.72 0.11 0.11) $ rect $ V2 w lavaHeight
    structure NoObstacle = blank
    structure Obstacle { .. } =
      move (V2 ((tx + bx) / 2) ((ty + by) / 2)) $ filled (rgb 0.38 0.49 0.55) $ rect $ V2 (bx - tx) (by - ty)

      where
        V2 tx ty = obsTopLeft
        V2 bx by = obsBottomRight

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
    }

  run engine defaultConfig GameLifecycle
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
