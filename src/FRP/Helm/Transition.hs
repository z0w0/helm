{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{-| Contains all data structures for describing transitions, composing and animating them. -}
module FRP.Helm.Transition (
  -- * Types
  Transition,
  TransitionStatus(..),
  Interpolate(..),
  -- * Creating
  waypoint,
  startWith,
  fromList,
  -- * Transitions
  transition,
  length
) where

import Control.Applicative
import FRP.Elerea.Simple
import FRP.Helm.Color (Color)
import FRP.Helm.Time (Time, inSeconds)
import Data.List (find)
import Prelude hiding (length)
import Data.Maybe (fromJust)
import GHC.Float
import Data.Word
import Data.Int
import GHC.Generics
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy

{-| A type describing a combosable transition. The writer keeps record of all the frames in the transition.
    The state holds the current value of the transition. This allows you to easily compose transitions using do notation. -}
type Transition a = StateT a (Writer [(a, Time)])

{-| This is used only for easier search of frames when transitioning is in progress. -}
data InternalFrame a =
  InternalFrame { -- | The initial value in the transition.
                  s :: a,
                  -- | The final value in the transition.
                  e :: a,
                  -- | The time that the transition will take.
                  t :: Double,
                  -- | The transition-relative time of the beginning of this frame.
                  tend :: Double,
                  -- | The transition-relative time of the end of this frame.
                  tstart :: Double
  } deriving Show

type InternalTransition a = [InternalFrame a]

{-| A variety of statuses that can be used to control a transition. -}
data TransitionStatus
  -- | The transition will repeat forever.
  = Cycle
  -- | The transition will be paused and won't changed until resumed.
  | Pause
  -- | The transition is cycled once and then stops.
  | Once
  -- | The transition will reset to a certain point in time.
  | Set Time

{-| Adds a value to the transition monad that will be the next point in the transition. -}
waypoint :: Interpolate a => a -> Time -> Transition a a
waypoint a t = do
  tell [(a, t)]
  put a
  return a

{-| Interpolates between the beginning and the end of the given frame. -}
transFrame :: Interpolate a => InternalFrame a -> Time -> a
transFrame InternalFrame{..} time = interpolate progress s e
  where
    progress = time / (tend - tstart)

{-| Searches the frame active at the given time and gives back the value of the frame at that time. -}
transitionAt :: Interpolate a => InternalTransition a -> Time -> a
transitionAt pks timeUnsafe = transFrame currentTransition currentTime
  where
    currentTime = time - tstart currentTransition
    currentTransition = fromJust $ find (\InternalFrame { .. } -> tend >= time) pks
    time = cycleTime pks timeUnsafe

{-| Turns the internal representation of a transition into a signal.
    The provided time signal acts as the inner clock of the transition.
    The status signal can be used to control the transition, deciding whether
    the transition should cycle, go to a specific time, pause, stop or run once. -}
transition :: Interpolate a => SignalGen (Signal Time) -> SignalGen (Signal TransitionStatus) -> InternalTransition a -> SignalGen (Signal a)
transition _ _ [] = error "empty transitions don't have any default value"
transition dtGen statusGen trans = do
  dt <- dtGen
  status <- statusGen
  time <- transfer2 0 step' status $ inSeconds <$> dt
  
  return $ transitionAt trans <$> time
  
  where
      step' Cycle dt t = cycleTime trans (dt + t)
      step' Pause _ t = t
      step' Once dt t = if newT < length trans then newT
                        else length trans
                        where newT = dt + t
      step' (Set t) _ _  = inSeconds t

{-| Converts a list of tuples describing a waypoint value and time into a transition.
    The first element in the list is the starting value and time of the transition.

    > color = transition (constant $ Time.fps 60) (constant Cycle) $ fromList [(white, 0), (green, 2 * seconds), (red, 5 * seconds), (black, 1 * seconds), (yellow, 2 * seconds)] -}
fromList :: Interpolate a => [(a,Time)] -> InternalTransition a
fromList [] = error "empty transitions don't have any default value"
fromList ((v1, d1) : xs) = scanl (\InternalFrame { .. } (v, d) -> InternalFrame e v d (tend + d) tend) first xs
  where
    first = InternalFrame v1 v1 d1 d1 0

{-| Starts a transition with an initial value. 

    > color = transition (constant $ Time.fps 60) (constant Cycle) $ startWith white $ do
    >   waypoint green (2 * seconds)
    >   waypoint red (5 * seconds)
    >   waypoint black (1 * seconds)
    >   waypoint yellow (2 * seconds)
-}
startWith :: Interpolate a => a -> Transition a b -> InternalTransition a
startWith beginning transitionMonad = fromList $ snd $ runWriter $ evalStateT (tell [(beginning, 0)] >> transitionMonad) beginning

{-| Given an animation, a function is created which loops the time of the animation
    to always be in the animations length boundary. -}
cycleTime :: InternalTransition a -> Time -> Time
cycleTime [] = const 0
cycleTime anim = cycleTime' (length anim)

{-| Helper function which makes a timer loop through an time interval. -}
cycleTime' :: Time -> Time -> Time
cycleTime' l t
  | t > l = cycleTime' l (t-l)
  | t < 0 = cycleTime' l (l+t)
  | otherwise = t

{-| How long it takes for the provided transition to end.  -}
length :: InternalTransition a -> Double
length = tend . last

{-| Defines a value that can be interpolated. An example instance of this class follows:

   > data YourDataType = YourDataConstructor SomeInterpolableType SomeOtherInterpolableType deriving Generic
   >
   > instance Interpolate YourDataType
   >   interpolate 0.5 (YourDataConstructor 3 5) (YourDataConstructor 5 7) == YourDataConstructor 4 6
 -}
class Interpolate a where
  interpolate :: Double -> a -> a -> a
  default interpolate :: (Generic a, GInterpolate (Rep a)) => Double -> a -> a -> a
  interpolate d a b = to $ ginterpolate d (from a) (from b)

class GInterpolate a where
  ginterpolate :: Double -> a b -> a b -> a b

instance GInterpolate V1 where
  ginterpolate _ _ b = b

instance GInterpolate U1 where
  ginterpolate _ _ b = b

instance (GInterpolate a, GInterpolate b) => GInterpolate (a :*: b) where
  ginterpolate d (a1 :*: b1) (a2 :*: b2) = ginterpolate d a1 a2 :*: ginterpolate d b1 b2

instance (GInterpolate a, GInterpolate b) => GInterpolate (a :+: b) where
  ginterpolate d (L1 a) (L1 b) = L1 $ ginterpolate d a b
  ginterpolate d (R1 a) (R1 b) = R1 $ ginterpolate d a b
  ginterpolate _ (L1 _) (R1 b) = R1 b
  ginterpolate _ (R1 _) (L1 b) = L1 b

instance (GInterpolate a) => GInterpolate (M1 i c a) where
  ginterpolate d (M1 a) (M1 b) = M1 $ ginterpolate d a b

instance (Interpolate a) => GInterpolate (K1 i a) where
  ginterpolate d (K1 a) (K1 b) = K1 $ interpolate d a b

instance Interpolate Double where
  interpolate p a b = b * p + a * (1-p)

instance Interpolate Float where
  interpolate p a b = b * double2Float p + a * double2Float (1 - p)

instance Interpolate Char where
  interpolate _ _ b = b

integralInterpolate :: Integral a => Double -> a -> a -> a
integralInterpolate d a b = ceiling $ interpolate d (fromIntegral a :: Double) (fromIntegral b :: Double)

instance Interpolate Word where
  interpolate = integralInterpolate

instance Interpolate Word8 where
  interpolate = integralInterpolate

instance Interpolate Word16 where
  interpolate = integralInterpolate

instance Interpolate Word32 where
  interpolate = integralInterpolate

instance Interpolate Word64 where
  interpolate = integralInterpolate

instance Interpolate Int where
  interpolate = integralInterpolate

instance Interpolate Int8 where
  interpolate = integralInterpolate

instance Interpolate Int16 where
  interpolate = integralInterpolate

instance Interpolate Int32 where
  interpolate = integralInterpolate

instance Interpolate Int64 where
  interpolate = integralInterpolate

instance Interpolate Integer where
  interpolate = integralInterpolate

instance Interpolate Bool
instance Interpolate (Double, Double)
instance Interpolate Color
