{-| Contains functions for composing units of time and signals that sample from the game clock. -}
module FRP.Helm.Time (
  -- * Units
  Time,
  millisecond,
  second,
  minute,
  hour,
  inMilliseconds,
  inSeconds,
  inMinutes,
  inHours,
  -- * Tickers
  fps,
  fpsWhen,
  every,
  -- * Timing
  timestamp,
  delay,
  since
) where

import Control.Applicative
import Control.Monad
import FRP.Elerea.Param hiding (delay, Signal, until)
import qualified FRP.Elerea.Param as Elerea (Signal, until)
import Data.Time.Clock.POSIX (getPOSIXTime)
import FRP.Helm.Signal
import FRP.Helm.Sample
import System.IO.Unsafe (unsafePerformIO)

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

{-| Takes desired number of frames per second (fps). The resulting signal gives
   a sequence of time deltas as quickly as possible until it reaches the
   desired FPS. A time delta is the time between the last frame and the current
   frame. -}
fps :: Double -> Signal Time
fps n = snd <~ every' t
  where --Ain't nobody got time for infinity
    t = if n == 0 then 0 else second / n

{-| Same as the fps function, but you can turn it on and off. Allows you to do
   brief animations based on user input without major inefficiencies. The first
   time delta after a pause is always zero, no matter how long the pause was.
   This way summing the deltas will actually give the amount of time that the
   output signal has been running. -}
fpsWhen :: Double -> Signal Bool -> Signal Time
fpsWhen n sig = Signal $ do c <- signalGen sig
                            f <- signalGen (fps n)
                            transfer2 (pure 0) update_ f c
  where update_ _ new (Unchanged cont) old = if cont
                                             then new
                                             else Unchanged $ value old
        update_ _ _   (Changed   cont) old = if cont
                                             then Changed 0
                                             else Unchanged $ value old
{-| Takes a time interval t. The resulting signal is the current time, updated
    every t. -}
every :: Time -> Signal Time
every t = fst <~ every' t

{-| A utility signal used by 'fps' and 'every' that returns the current time
    and a delta every t. -}
every' :: Time -> Signal (Time, Time)
every' t = Signal $ every'' t >>= transfer (pure (0,0)) update

{-| Another utility signal that does all the magic for 'every'' by working on
    the Elerea SignalGen level -}
every'' :: Time -> SignalGen p (Elerea.Signal (Time, Time))
every'' t = do
    it <- execute getTime
    effectful getTime >>= transfer (it,0) update_
  where
    getTime = liftM ((second *) . realToFrac) getPOSIXTime
    update_ _ new old = let delta = new - fst old
                        in if delta >= t then (new, delta) else old

{-| Add a timestamp to any signal. Timestamps increase monotonically. When you
    create (timestamp Mouse.x), an initial timestamp is produced. The timestamp
    updates whenever Mouse.x updates.

    Unlike in Elm the timestamps are not tied to the underlying signals so the
    timestamps for Mouse.x and  Mouse.y will be slightly different. -}
timestamp :: Signal a -> Signal (Time, a)
timestamp = lift2 (,) pure_time
  where pure_time = fst <~ (Signal $ (fmap . fmap) pure (every'' millisecond))

{-| Delay a signal by a certain amount of time. So (delay second Mouse.clicks)
    will update one second later than any mouse click. -}
delay :: Time -> Signal a -> Signal a
delay t (Signal gen) = Signal $ (fmap . fmap) fst $
                         do s <- gen
                            w <- timeout
                            e <- snapshot =<< input
                            transfer2 (makeInit e, []) update_ w s
  where
     -- XXX uses unsafePerformIO, is there a better way?
    makeInit e = pure $ value $ unsafePerformIO (start gen >>= (\f -> f e))
    update_ _ waiting new (old, olds) = if waiting then (old, new:olds)
                                        else (last olds, new:init olds)
    timeout = every'' t >>= transfer False (\_ (time,delta) _ -> time /= delta)
                        -- 'Elerea.until' will lose the reference to the input so
                        -- we don't keep looking up the time even though the
                        -- output can never change again
                        >>= Elerea.until
                        >>= transfer True (\_ new old -> old && not new)

{-| Takes a time t and any signal. The resulting boolean signal is true for
    time t after every event on the input signal. So (second `since`
    Mouse.clicks) would result in a signal that is true for one second after
    each mouse click and false otherwise. -}
since :: Time -> Signal a -> Signal Bool
since t s = lift2 (/=) (count s) (count (delay t s))
