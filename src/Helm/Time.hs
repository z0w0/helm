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
  ) where

import Control.Monad.State (get)
import Control.Monad.IO.Class (liftIO)

import Helm.Engine (Cmd(..), Sub(..), Engine(..))

-- | A type describing an amount of time in an arbitary unit.
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
now ::
  Engine e
  => (Time -> a)  -- ^ The function to map the running time to an action.
  -> Cmd e a      -- ^ The mapped command.
now f = Cmd $ do
  engine <- get
  ticks <- liftIO $ f <$> runningTime engine

  return [ticks]

-- | Subscribe to the running time of the engine and map to a game action,
-- producing events at a provided interval.
every ::
  Engine e
  => Time         -- ^ The interval of time to produce events at.
  -> (Time -> a)  -- ^ The function to map the running time to an action.
  -> Sub e a      -- ^ The mapped subscription.
every _ _ = Sub $ return $ return []
