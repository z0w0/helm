module FRP.Helm.Animation(
  Frame,
  Animation(),
  animate,
  createAnimation,
  currentForm,
  animLength
)where

import FRP.Elerea.Simple
import Control.Applicative
import FRP.Helm.Graphics (Form)
import FRP.Helm.Time (Time)
import Data.Maybe(fromJust)
import qualified Data.List as L

{-|One picture of an animation. (How long this picture is shown,picture) |-}
type Frame = (Time,Form)

{-|A wrapper for safety.|-}
newtype Animation = Animation [Frame] deriving (Show,Eq)

{-| Each input Frame describes one picture and how long it is shown as part of an animation.
    The collection of Frames is then converted into a newtype wrapper to prevent mistakes.
    The Time values are also translatet into abolute time in respect to the animation.
    [(100,picture1),(100,picture2),(300,picture3)] -> [(100,picture1),(200,picture2),(500,picture3)]
    |-}
createAnimation :: [Frame] -> Animation
createAnimation frames = Animation $ L.scanl1 (\acc x-> (fst acc+fst x, snd x) ) frames


{-| This turns your Animation data into a Signal with changing pictures!
    More Time,more pictures. Reset button included.
    The input Animation is advanced by the input Time value every time this Signal is sampled.
    The animation restarts every time the input Bool value is False if the Signal is sampled. |-}
animate :: Animation -> Signal Time -> Signal Bool -> SignalGen (Signal Form)
animate anim dt reset = do
  progress <- transfer2 0 (\t r animT-> if r then t else resetThisAnim (animT+t) ) dt reset
  return $ (currentForm anim) <$> progress
    where
      resetThisAnim = resetOnEnd anim

{-| The current snapshot of the animation at the given time 'inside of the animation'. |-}
currentForm :: Animation -> Time -> Form
currentForm (Animation anim) t = snd $ fromJust $ L.find (\frame -> t < (fst frame) ) anim


{-| How long one cylce of the animation takes.  |-}
animLength :: Animation -> Time
animLength = L.maximum . animTimes


{-| All the time values of an Animation. |-}
animTimes :: Animation -> [Time]
animTimes (Animation anim) = L.map fst anim


{-| Given an Animation, a function is created,
    which resets the time of the animation if the animation was finished. |-}
resetOnEnd :: Animation -> (Time -> Time)
resetOnEnd anim = resetOnEnd' (animLength anim)


{-| Helper function which resets a timer if the timer got bigger than a given number. |-}
resetOnEnd' :: Time -> Time -> Time
resetOnEnd' l t
  | t >= l = 0
  | otherwise = t