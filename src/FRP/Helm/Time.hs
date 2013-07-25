{-| Contains types for composing units of time and sampling values from the game clock. -}
module FRP.Helm.Time (
  -- * Types
  Time,
  -- * Composing
  millisecond,
  second,
  minute,
  hour,
  inMilliseconds,
  inSeconds,
  inMinutes,
  inHours,
  -- * Clock State
  running,
  delta,
  delay
) where

import Control.Applicative
import FRP.Elerea.Simple hiding (delay)
import FRP.Helm (Time)
import qualified Graphics.UI.SDL as SDL

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
hour = 60000

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

{-| A signal that returns the time that the game has been running for when sampled. -}
running :: SignalGen (Signal Time)
running = effectful $ realToFrac <$> SDL.getTicks

{-| A signal that returns the time since it was last sampled when sampled. -}
delta :: SignalGen (Signal Time)
delta = running >>= delta'

{-| A utility function that does the real magic for 'delta'. -}
delta' :: Signal Time -> SignalGen (Signal Time)
delta' t = (fmap . fmap) snd $ transfer (0, 0) (\t2 (t1, _) -> (t2, t2 - t1)) t

{-| A signal that blocks the game thread for a certain amount of time when sampled and then returns the
    amount of time it blocked for. Please note that delaying by values smaller than 1 millisecond can have
    platform-specific results. -}
delay :: Time -> SignalGen (Signal Time)
delay t = effectful $ SDL.delay fixed >> return (realToFrac fixed)
  where
    fixed = max 0 $ round t
