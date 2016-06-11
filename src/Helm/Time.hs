{-| Contains functions for composing units of time and
    subscriptions to events from the game clock. -}
module Helm.Time (
  -- * Types
  Time,
  -- * Units
  millisecond,
  second,
  minute,
  hour,
  inMilliseconds,
  inSeconds,
  inMinutes,
  inHours,
  -- * Commands
  now,
  -- * Subscriptions
  every,
) where

import Helm (Cmd(..), Sub(..))

{-| A type describing an amount of time in an arbitary unit. Use the time
    composing/converting functions to manipulate time values. -}
type Time = Double

{-| A time value representing one millisecond. -}
millisecond :: Time
millisecond = 1

{-| A time value representing one second. -}
second :: Time
second = 1000

{-| A time value representing one minute. -}
minute :: Time
minute = 60000

{-| A time value representing one hour. -}
hour :: Time
hour = 3600000

{-| Converts a time value to a fractional value, in milliseconds. -}
inMilliseconds :: Time -> Double
inMilliseconds n = n

{-| Converts a time value to a fractional value, in seconds. -}
inSeconds :: Time -> Double
inSeconds n = n / second

{-| Converts a time value to a fractional value, in minutes. -}
inMinutes :: Time -> Double
inMinutes n = n / minute

{-| Converts a time value to a fractional value, in hours. -}
inHours :: Time -> Double
inHours n = n / hour

now :: Cmd Time
now = Cmd $ return []

every :: Time -> (Time -> a) -> Sub a
every _ _ = Sub $ return $ return []
