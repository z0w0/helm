{-| Contains all data structures and functions for creating and stepping animations. -}
module FRP.Helm.Animation (
  -- * Types
  Frame,
  Animation,
  AnimationStatus(..),
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
import FRP.Helm.Time (Time, inMilliseconds)
import Data.Maybe (fromJust)
import Data.List (find)
import qualified Data.List as List (length)

{-| A type describing a single frame in an animation. A frame consists of a time at
    which the frame takes place in an animation and the form which is how the frame
    actually looks when rendered. -}
type Frame = (Time, Form)

{-| A type describing an animation consisting of a list of frames. -}
type Animation = [Frame]

{-| A data structure that can be used to manage the status of the animation. -}
data AnimationStatus
  -- | The animation continues to play through its frames.
  = Cycle
  -- | The animation is paused.
  | Pause
  -- | The animation is stopped, jumping back to the first frame and initial time.
  | Stop
  -- | The animation is set to a specific one-indexed frame.
  | SetFrame Int
  -- | The animation is set to a specific time and its related frame.
  | SetTime Time

{-| Creates an animation from a list of frames. The time value in each frame
    is absolute to the entire animation, i.e. each time value is the time
    at which the frame takes place relative to the starting time of the animation. -}
absolute :: [Frame] -> Animation
absolute = id

{-| Creates an animation from a list of frames. The time value in each frame
    is relative to other frames, i.e. each time value is the difference
    in time from the last frame.

    > relative [(100 * milliseconds, picture1), (100 * milliseconds, picture2)] == absolute [(100 * milliseconds, picture1), (200 * milliseconds, picture2)]
 -}
relative :: [Frame] -> Animation
relative = scanl1 (\acc x -> (fst acc + fst x, snd x))

{-| Creates a signal that returns the current form in the animation when sampled from
    a specific animation. The second argument is a signal signal that returns the time to
    setup the animation forward when sampled. The third argument is a signal that returns
    the status of the animation, allowing you to control it. -}
animate :: Animation -> SignalGen (Signal Time) -> SignalGen (Signal AnimationStatus) -> SignalGen (Signal Form)
animate [] _ _ = return $ return blank
animate anim dt status = do
  dt1 <- dt
  status1 <- status
  progress <- transfer2 0 (timestep anim) status1 $ inMilliseconds <$> dt1

  return $ fromJust <$> formAt anim <$> progress

{-| Steps the animation but also cycles if the end is reached, handles any statuses and
    tries to pickup any issues and handle them silently. -}
timestep :: Animation -> AnimationStatus -> Time -> Time -> Time
timestep anim Cycle dt t = cycleTime anim (dt + t)
timestep _ Pause _ t = t
timestep _ Stop _ _ = 0
timestep anim (SetTime sT) _ _ = cycleTime anim $ inMilliseconds sT
timestep anim (SetFrame f) _ _ = gentleIndex anim f
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
