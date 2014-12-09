module FRP.Helm.Signal(
  Signal(..),
  -- * Composing
  constant,
  combine,
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
import Data.Traversable (sequenceA)
import FRP.Elerea.Param hiding (Signal)
import qualified FRP.Elerea.Param as Elerea (Signal)
import FRP.Helm.Sample
import FRP.Helm.Engine

newtype Signal a = Signal {signalGen :: SignalGen Engine (Elerea.Signal (Sample a))}

instance Functor Signal where
  fmap = liftA

instance Applicative Signal where
  pure = Signal . pure . pure . pure
  (Signal f) <*> (Signal x) = Signal $ liftA2 (liftA2 (<*>)) f x

{-| Creates a signal that never changes. -}
constant :: a -> Signal a
constant x = Signal $ stateful (Changed x) (\_ _ -> Unchanged x)

{-| Combines a list of signals into a signal of lists. -}
combine :: [Signal a] -> Signal [a]
combine = sequenceA

{-| Applies a function to a signal producing a new signal. This is a synonym of
   'fmap'. It automatically binds the input signal out of the signal generator.

    > lift render Window.dimensions
 -}
lift :: (a -> b) -> Signal a -> Signal b
lift = fmap

{-| Applies a function to two signals. -}
lift2 :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift2 f a b = f <~ a ~~ b

{-| Applies a function to three signals. -}
lift3 :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
lift3 f a b c = f <~ a ~~ b ~~ c

{-| Applies a function to four signals. -}
lift4 :: (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d
                                 -> Signal e
lift4 f a b c d = f <~ a ~~ b ~~ c ~~ d

{-| Applies a function to five signals. -}
lift5 :: (a -> b -> c -> d -> e -> f) -> Signal a -> Signal b -> Signal c -> Signal d
                                      -> Signal e -> Signal f
lift5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

{-| Applies a function to six signals. -}
lift6 :: (a -> b -> c -> d -> e -> f -> g) -> Signal a -> Signal b -> Signal c -> Signal d
                                           -> Signal e -> Signal f -> Signal g
lift6 f a b c d e f1 = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1

{-| Applies a function to seven signals. -}
lift7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Signal a -> Signal b -> Signal c -> Signal d
                                                -> Signal e -> Signal f -> Signal g -> Signal h
lift7 f a b c d e f1 g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1 ~~ g

{-| Applies a function to eight signals. -}
lift8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Signal a -> Signal b -> Signal c -> Signal d
                                                     -> Signal e -> Signal f -> Signal g -> Signal h
                                                     -> Signal i
lift8 f a b c d e f1 g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1 ~~ g ~~ h

{-| An alias for 'lift'. -}
(<~) :: (a -> b) -> Signal a -> Signal b
(<~) = lift

infixl 4 <~

{-| Applies a function within a signal to a signal. This is a synonym of <*>.
    It automatically binds the input signal out of the signal generator.

    > render <~ Window.dimensions ~~ Window.position
 -}
(~~) :: Signal (a -> b) -> Signal a -> Signal b
(~~) = (<*>)

infixl 4 ~~

{-| Creates a past-dependent signal that depends on another signal. This is a
    wrapper around the 'transfer' function that automatically binds the input
    signal out of the signal generator. This function is useful for making a render
    function that depends on some accumulated state.
    
    > playerPosition :: (Int, Int) -> SignalGen (Signal (Int, Int))
    > playerPosition initial = foldp update initial arrows
    >     where update (dx, dy) (x, y) = (x + dx, y + dy)

-}
foldp :: (a -> b -> b) -> b -> Signal a -> Signal b
foldp f ini (Signal gen) =
  Signal $ gen >>= transfer (pure ini) update_
               >>= delay (Changed ini)
    where update_ _ (Unchanged _) y = Unchanged (value y)
          update_ _ (Changed   x) y = Changed $ f x (value y)

{-| Count the number of events that have occurred.-}
count :: Signal a -> Signal Int
count = foldp (\_ y -> y + 1) 0

{-| Count the number of events that have occurred that satisfy a given predicate.-}
countIf :: (a -> Bool) -> Signal a -> Signal Int
countIf f = foldp (\v c -> c + fromEnum (f v)) 0
