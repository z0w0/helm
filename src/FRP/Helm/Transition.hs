{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances #-}
{- With this module you can interpolate any value and create transitions which are interpolations over time.
   >> changingColor = startWith white $ do
   >>   waypoint green 2
   >>   waypoint red 5
   >>   waypoint black 1
   >>   waypoint yellow 2
   or
   >> changingColor = fromList [(white,0),(green,2),(red,5),(black,1),(yellow,2)]
   This plays the transition once at the start of the game.
   >> colorSignal <- transition changingColor delta (return $ return Once)
  -}
module FRP.Helm.Transition (
  -- * Types
  Transition,
  Status(..),
  Interpolate,
  -- * Creating
  waypoint,
  startWith,
  fromList,
  -- * Transitions
  transition,
  interpolate,
  length
  )where

import Control.Applicative
import FRP.Elerea.Simple
import FRP.Helm.Color(Color)
import FRP.Helm.Time(Time)
import Data.List(find)
import Prelude hiding (length)
import Data.Maybe(fromJust)
import GHC.Float
import Data.Word
import Data.Int
import GHC.Generics
import Control.Monad.Writer.Lazy
import Control.Monad.State.Lazy

{- This is used for creation and composition of Transitions. 
   The Writer is supposed to keep record of all the Frames.
   The time is in seconds.
   The state is supossed to hold the current value of the transition.
   The state is there only so you can use do notation without passing the arguments explicitly.
  -}
type Transition a = StateT a (Writer [(a,Time)])

{- This is used only for easier search of frames when transitioning is in progress. 
   s: start value
   e: end value
   t: time the transition is to take
   tstart: transition relative time of the beginning of this frame
   tend: transition relative time of the end of this frame
  -}
data InternalFrame a = InternalFrame {s :: a,
                                      e :: a,
                                      t :: Double,
                                      tend :: Double,
                                      tstart :: Double
                                      } deriving Show

type InternalTransition a = [InternalFrame a]

{- This can be used to control a transition.
   Cycle: The transition repeats forever.
   Pause: A paused transition doesn't change.
   Once: The transition is done once and stops at the last value.
   Set: The transition is set to the specified time in seconds. -}
data Status = Cycle | Pause | Once | Set Time

{- Adds a value to the Transition a Monad, which will be part of the transition. -}
waypoint :: Interpolate a => a -> Time -> Transition a a
waypoint a t = do
  tell [(a,t)]
  put a
  return a

{- Interpolates between the beginning and the end of the given frame.
  -}
transFrame :: Interpolate a => InternalFrame a -> Time -> a
transFrame InternalFrame{..} time = interpolate progress s e
  where
    progress = time / (tend - tstart)

{- Searches the frame active at the given time and gives back the value of the frame at that time.
  -}
transitionAt :: Interpolate a => InternalTransition a -> Time -> a
transitionAt pks timeUnsafe = transFrame currentTransition currentTime
  where
    currentTime = time - tstart currentTransition
    currentTransition = fromJust $ find (\InternalFrame{..}-> tend >= time) pks
    time = cycleTime pks timeUnsafe

{- Turns the internal representation of a Transition into a Signal representing the transition.
   The time SignalGen acts as the inner clock of the transition. The given time signal is assumed to be in milliseconds.
   The Status SignalGen decides wether the transition shall cylce, pause, stop or run once.
  -}
transition :: Interpolate a => InternalTransition a -> SignalGen(Signal Time) -> SignalGen (Signal Status) -> SignalGen(Signal a)
transition []    _     _         = error "empty transitions don't have any default value"
transition trans dtGen statusGen = do
  dt <- dtGen
  status <- statusGen
  time <- transfer2 0 step' status dt
  return $ transitionAt trans <$> time
  where
      step' Cycle   dt t = cycleTime trans (dt/1000+t)
      step' Pause   _  t = t
      step' Once    dt t = let newT = dt/1000+t in if newT < length trans then newT else length trans
      step' (Set t) _ _  = t

{- The [(a,Time)] are convertet to InternalTransition a, so they are easier to interpolate through.
   The (a,Time) pairs represent the values to which should be interpolatet and how long this interpolation should take.
   The transition begins with the first (a,_) of the list, where the transition waits (_,time) long. 
  -}
fromList :: Interpolate a => [(a,Time)] -> InternalTransition a
fromList [] = error "empty transitions don't have any default value"
fromList ((v1,d1):xs) = scanl (\InternalFrame{..} (v, d) -> InternalFrame e v d (tend+d) tend ) first xs
  where
    first = InternalFrame v1 v1 d1 d1 0

{- The given transition is started from a given start value.
  -}
startWith :: Interpolate a => a -> Transition a b -> InternalTransition a
startWith beginning transitionMonad = fromList $ snd $ runWriter $ evalStateT (tell [(beginning,0)] >> transitionMonad) beginning

{-| Given an animation, a function is created which loops the time of the animation
    to always be in the animations length boundary.
  -}
cycleTime :: InternalTransition a -> Time -> Time
cycleTime [] = const 0
cycleTime anim = cycleTime' (length anim)

{-| Helper function which makes a timer loop through an time interval. -}
cycleTime' :: Time -> Time -> Time
cycleTime' l t
  | t > l = cycleTime' l (t-l)
  | t < 0 = cycleTime' l (l+t)
  | otherwise = t

{- How long it takes a transition to end. 
  -}
length :: InternalTransition a -> Double
length = tend . last

{- Members of this class can be interpolated.
   Default instances for Datatypes comprised of interpolatable values can be generated like this:
   > data YourDataType = YourDataConstructor SomeInterpolableType SomeOtherInterpolableType deriving Generic
   > instance Interpolate YourDataType
   > interpolate 0.5 (YourDataConstructor 3 5) (YourDataConstructor 5 7) == YourDataConstructor 4 6
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

instance (GInterpolate a,GInterpolate b) => GInterpolate (a :*: b) where
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
  interpolate p a b = b * double2Float p + a * double2Float (1-p)

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
instance Interpolate (Double,Double)
instance Interpolate Color
