{-# LANGUAGE ExistentialQuantification #-}
module FRP.Helm.Signal(
  Signal(..),
  -- * Composing
  constant,
  combine,
  merge,
  mergeMany,
  sampleOn,
  lift,
  lift2,
  lift3,
  (<~),
  (~~),
  -- * Accumulating
  foldp,
  count,
  countIf,
  -- * DYEL?
  lift4,
  lift5,
  lift6,
  lift7,
  lift8
) where
import Control.Applicative

import FRP.Elerea.Param hiding (Signal)
import qualified FRP.Elerea.Param as Elerea (Signal)
import FRP.Helm.Sample

newtype Signal engine a = Signal {signalGen :: SignalGen engine (Elerea.Signal (Sample a))}

instance Functor (Signal engine) where
  fmap = liftA

instance Applicative (Signal engine) where
  pure = Signal . pure . pure . pure
  (Signal f) <*> (Signal x) = Signal $ liftA2 (liftA2 (<*>)) f x

{-| Creates a signal that never changes. -}
constant :: a -> Signal engine a
constant x = Signal $ stateful (Changed x) (\_ _ -> Unchanged x)

{-| Combines a list of signals into a signal of lists. -}
combine :: [Signal engine a] -> Signal engine [a]
combine = sequenceA

{-|
Merge two signals into one. This function is extremely useful for bringing
together lots of different signals to feed into a foldp.

If an update comes from either of the incoming signals, it updates the outgoing
signal. If an update comes on both signals at the same time, the left update
wins (i.e., the right update is discarded).
-}
merge :: Signal engine a -> Signal engine a -> Signal engine a
merge s1 s2 = Signal $ do
  s1' <- signalGen s1
  s2' <- signalGen s2
  return $ update' <$> s1' <*> s2'
    where update' (Changed   x) _             = Changed x
          update' (Unchanged _) (Changed y)   = Changed y
          update' (Unchanged x) (Unchanged _) = Unchanged x

{-| Merge many signals into one. This is useful when you are merging more than
   two signals. When multiple updates come in at the same time, the left-most
   update wins, just like with merge. -}
mergeMany :: [Signal engine a] -> Signal engine a
mergeMany = foldl1 merge

{-| Sample the second signal based on the first. -}
sampleOn :: Signal engine a -> Signal engine b -> Signal engine b
sampleOn s1 s2 = Signal $ do
  s1' <- signalGen s1
  s2' <- signalGen s2
  return $ update' <$> s1' <*> s2'
    where update' (Unchanged _) (Changed   y) = Unchanged y
          update' (Unchanged _) (Unchanged y) = Unchanged y
          update' (Changed   _) (Changed   y) = Changed y
          update' (Changed   _) (Unchanged y) = Changed y

{-| Applies a function to a signal producing a new signal. This is a synonym of
   'fmap'. It automatically binds the input signal out of the signal generator.

    > lift render Window.dimensions
 -}
lift :: (a -> b) -> Signal engine a -> Signal engine b
lift = fmap

{-| Applies a function to two signals. -}
lift2 :: (a -> b -> c) -> Signal engine a -> Signal engine b -> Signal engine c
lift2 f a b = f <~ a ~~ b

{-| Applies a function to three signals. -}
lift3 :: (a -> b -> c -> d) -> Signal engine a -> Signal engine b
                            -> Signal engine c -> Signal engine d
lift3 f a b c = f <~ a ~~ b ~~ c

{-| Applies a function to four signals. -}
lift4 :: (a -> b -> c -> d -> e) -> Signal engine a -> Signal engine b -> Signal engine c
                                 -> Signal engine d -> Signal engine e
lift4 f a b c d = f <~ a ~~ b ~~ c ~~ d

{-| Applies a function to five signals. -}
lift5 :: (a -> b -> c -> d -> e -> f)
            -> Signal engine a -> Signal engine b -> Signal engine c
            -> Signal engine d -> Signal engine e -> Signal engine f
lift5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

{-| Applies a function to six signals. -}
lift6 :: (a -> b -> c -> d -> e -> f -> g)
            -> Signal engine a -> Signal engine b -> Signal engine c -> Signal engine d
            -> Signal engine e -> Signal engine f -> Signal engine g
lift6 f a b c d e f1 = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1

{-| Applies a function to seven signals. -}
lift7 :: (a -> b -> c -> d -> e -> f -> g -> h)
            -> Signal engine a -> Signal engine b -> Signal engine c-> Signal engine d
            -> Signal engine e -> Signal engine f -> Signal engine g -> Signal engine h
lift7 f a b c d e f1 g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1 ~~ g

{-| Applies a function to eight signals. -}
lift8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i)
            -> Signal engine a -> Signal engine b -> Signal engine c -> Signal engine d
            -> Signal engine e -> Signal engine f -> Signal engine g -> Signal engine h
            -> Signal engine i
lift8 f a b c d e f1 g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1 ~~ g ~~ h

{-| An alias for 'lift'. -}
(<~) :: (a -> b) -> Signal engine a -> Signal engine b
(<~) = lift

infixl 4 <~

{-| Applies a function within a signal to a signal. This is a synonym of <*>.
    It automatically binds the input signal out of the signal generator.

    > render <~ Window.dimensions ~~ Window.position
 -}
(~~) :: Signal engine (a -> b) -> Signal engine a -> Signal engine b
(~~) = (<*>)

infixl 4 ~~

{-| Creates a past-dependent signal that depends on another signal. This is a
    wrapper around the 'transfer' function that automatically binds the input
    signal out of the signal generator. This function is useful for making a render
    function that depends on some accumulated state.

    > playerPosition :: (Int, Int) -> Signal (Int, Int)
    > playerPosition initial = foldp update initial arrows
    >     where update (dx, dy) (x, y) = (x + dx, y + dy)

-}
foldp :: (a -> b -> b) -> b -> Signal engine a -> Signal engine b
foldp f ini (Signal gen) =
  Signal $ gen >>= transfer (pure ini) update_
               >>= delay (Changed ini)
    where update_ _ (Unchanged _) y = Unchanged (value y)
          update_ _ (Changed   x) y = Changed $ f x (value y)

{-| Count the number of events that have occurred.-}
count :: Signal engine a -> Signal engine Int
count = foldp (\_ y -> y + 1) 0

{-| Count the number of events that have occurred that satisfy a given predicate.-}
countIf :: (a -> Bool) -> Signal engine a -> Signal engine Int
countIf f = foldp (\v c -> c + fromEnum (f v)) 0
