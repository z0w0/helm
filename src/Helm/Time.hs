-- | Contains functions for composing units of time and
-- subscriptions to events from the game clock.
module Helm.Time
  (
    -- * Types
    Time
    -- * Units
  , millisecond
  , second
  , minute
  , hour
  , inMilliseconds
  , inSeconds
  , inMinutes
  , inHours
    -- * Commands
  , now
    -- * Subscriptions
  , every
  , fps
  ) where

import Control.Monad.State (get)
import Control.Monad.IO.Class (liftIO)

import FRP.Elerea.Param (transfer, input, snapshot, effectful)

import Helm.Engine (Cmd(..), Sub(..), Engine(..))

-- | Represents an amount of time in an arbitary unit.
-- This type can then be composed with the relevant utility functions.
type Time = Double

-- | A time value representing one millisecond.
millisecond :: Time
millisecond = 1

-- | A time value representing one second.
second :: Time
second = 1000

-- | A time value representing one minute.
minute :: Time
minute = 60000

-- | A time value representing one hour.
hour :: Time
hour = 3600000

-- | Converts a time value to a fractional value, in milliseconds.
inMilliseconds :: Time -> Double
inMilliseconds n = n

-- | Converts a time value to a fractional value, in seconds.
inSeconds :: Time -> Double
inSeconds n = n / second

-- | Converts a time value to a fractional value, in minutes.
inMinutes :: Time -> Double
inMinutes n = n / minute

-- | Converts a time value to a fractional value, in hours.
inHours :: Time -> Double
inHours n = n / hour

-- | Map the running time of the engine to a game action.
-- Note that this is not the current clock time but rather the engine time,
-- i.e. when the engine first starts running, the applied value will be zero.
now
  :: Engine e
  => (Time -> a)  -- ^ The function to map the running time to an action.
  -> Cmd e a      -- ^ The mapped command.
now f = Cmd $ do
  engine <- get
  ticks <- liftIO $ f <$> runningTime engine

  return [ticks]

-- | Generic subscription for time interval-based events.
every' :: Engine e => (e -> Time -> (Time, [Double]) -> (Time, [Double])) -> (Time -> a) -> Sub e a
every' step f = Sub $ do
  engine <- input >>= snapshot
  time <- effectful $ runningTime engine
  sig <- transfer (0, []) step time

  return $ map f . snd <$> sig

-- | Subscribe to the running time of the engine and map to a game action,
-- producing events at a provided interval.
every
  :: Engine e
  => Time         -- ^ The interval of time to produce events at.
  -> (Time -> a)  -- ^ The function to map the running time to an action.
  -> Sub e a      -- ^ The mapped subscription.
every interval = every' step
  where
    step _ t (lt, _) =
      if t - lt >= interval
      then (t, [t])  -- Use new t value with t event
      else (lt, [])  -- Use old t value with no event

-- | Subscribe to events that emit at a provided frames per second and map to a game action,
-- producing events at a provided interval. The time value applied is the delta between
-- the current and last event emission time.
fps
  :: Engine e
  => Int          -- ^ The frames per second.
  -> (Time -> a)  -- ^ The function to map the time delta to an action.
  -> Sub e a      -- ^ The mapped subscription.
fps frames = every' step
  where
    interval = 1000 / fromIntegral frames
    step _ t (lt, _) =
      if t - lt >= interval
      then (t, [t - lt])  -- Use new t value with t event
      else (lt, [])  -- Use old t value with no event
