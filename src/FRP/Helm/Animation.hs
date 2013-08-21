{-| Contains all data structures and functions for creating and stepping animations. -}
module FRP.Helm.Animation (
  -- * Types
  Frame,
  Animation,
  Status(..),
  -- * Creating
  absolute,
  relative,
  -- * Animating
  animate,
  formAt,
  length
) where

import Prelude hiding (length)

import FRP.Elerea.Simple
import Control.Applicative
import FRP.Helm.Graphics (Form,blank)
import FRP.Helm.Time (Time)
import Data.Maybe (fromJust)
import Data.List (find)
import qualified Data.List as List (length)

{-| A type describing a single frame in an animation. A frame consists of a time at
    which the frame takes place in an animation and the form which is how the frame
    actually looks when rendered. -}
type Frame = (Time, Form)


{-| A type describing an animation consisting of a list of frames. -}
type Animation = [Frame]

{-| This type tells of the state an animation is in.
    Continue: A continued animation plays through its frames as specified in the Animation.
    Pause: A paused animation does not change its current frame and time.
    Stop: A stopped animation is set to its first frame and time 0.
    Frame: The Frame constructor can be used to choose a specific frame of the animation
           where time is set to the first millisecond of that chosen frame. (Indexing starts at 1. 'first frame', not 'zero frame')
    Time: The Time constructor sets the current time (in milliseconds) in the animation to the specified value.   -}
data Status = Continue | Pause | Stop | Frame Int | Time Time

{-| Creates an animation from a list of frames. The time value in each frame
    is absolute to the entire animation, i.e. each time value is the time
    at which the frame takes place relative to the starting time of the animation.
 -}
absolute :: [Frame] -> Animation
absolute = id

{-| Creates an animation from a list of frames. The time value in each frame
    is relative to other frames, i.e. each time value is the difference
    in time from the last frame.

    > relative [(100, picture1), (100, picture2), (300, picture3)] == absolute [(100, picture1), (200, picture2), (500, picture3)]
 -}
relative :: [Frame] -> Animation
relative = scanl1 (\acc x -> (fst acc + fst x, snd x))

{-| Creates a signal contained in a generator that returns the current form in the animation when sampled from
    a specific animation. The second argument is a signal generator containing a signal that
    returns the time to setup the animation forward when sampled. The third argument is a
    signal generator containing a signal that returns true to continue animating
    or false to stop animating when sampled. -}
animate :: Animation -> SignalGen (Signal Time) -> SignalGen (Signal Status) -> SignalGen (Signal Form)
animate [] _ _ = return $ return blank
animate anim dt status = do
  dt1 <- dt
  status1 <- status
  progress <- transfer2 0 (timestep anim) status1 dt1

  return $ fromJust <$> formAt anim <$> progress

{-| Makes a step in the Animation of size dt, but also takes care to:
    start over if the end was reached.
    behave in accordance to the possible statuses,
    handle erroneous input gently (this may do more bad than good since it may be unexpected, please dispute!) -}
timestep :: Animation -> Status -> Time -> Time -> Time
timestep anim Continue  dt t = cycleTime anim (dt + t)
timestep _    Pause     _  t = t
timestep _    Stop      _  _ = 0
timestep anim (Time sT) _  _ = cycleTime anim sT
timestep anim (Frame f) _  _ = gentleIndex anim f
  where
    gentleIndex [] _ = 0
    gentleIndex xs n = fst $ xs !! (cycleFrames anim n -1)

{-| The form that will be rendered for a specific time in an animation. -}
formAt :: Animation -> Time -> Maybe Form
formAt anim t = snd <$> find (\frame -> t <= fst frame) anim

{-| The amount of time one cycle of the animation takes. -}
length :: Animation -> Time
length [] = 0
length anim = maximum $ times anim

{-| A list of all the time values of each frame in the animation. -}
times :: Animation -> [Time]
times = map fst

{-| Given an animation, a function is created which loops the time of the animation
    to always be in the animations length boundary. -}
cycleTime :: Animation -> Time -> Time
cycleTime anim = cycleTime' (length anim)

{-| Helper function which makes a timer loop through an time interval. -}
cycleTime' :: Time -> Time -> Time
cycleTime' l t
  | t > l = cycleTime' l (t-l)
  | t < 0 = cycleTime' l (l+t)
  | otherwise = t

{-| Given an animation, a function is created which loops the frame indices of the animation
    to always be in the animations frame length boundary. -}
cycleFrames :: Animation -> Int -> Int
cycleFrames anim = cycleFrames' (List.length anim)

{-| Helper function which makes a frame index loop through an interval starting at 1. -}
cycleFrames' :: Int -> Int -> Int
cycleFrames' l f
  | f > l = cycleFrames' l (f-l)
  | f < 1 = cycleFrames' l (l+f)
  | otherwise = f
