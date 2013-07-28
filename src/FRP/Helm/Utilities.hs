{-| Contains utility functions including functions for working with signals and signal generators. -}
module FRP.Helm.Utilities (
  -- * Composing
  constant,
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

import Control.Applicative ((<*>))
import FRP.Elerea.Simple

{-| Creates a signal that never changes. -}
constant :: a -> SignalGen (Signal a)
constant value = return $ return value

{- TODO:
combine :: [SignalGen (Signal a)] -> SignalGen (Signal [a])
-}

{-| Applies a function to a signal producing a new signal. This is a wrapper around the builtin
    'fmap' function that automatically binds the input signal out of the signal generator.

    > render <~ Window.dimensions
 -}
lift :: (a -> b) -> SignalGen (Signal a) -> SignalGen (Signal b)
lift f = fmap (fmap f)

{-| Applies a function to two signals. -}
lift2 :: (a -> b -> c) -> SignalGen (Signal a) -> SignalGen (Signal b) -> SignalGen (Signal c)
lift2 f a b = f <~ a ~~ b

{-| Applies a function to three signals. -}
lift3 :: (a -> b -> c -> d) -> SignalGen (Signal a) -> SignalGen (Signal b) -> SignalGen (Signal c) -> SignalGen (Signal d)
lift3 f a b c = f <~ a ~~ b ~~ c

{-| Applies a function to four signals. -}
lift4 :: (a -> b -> c -> d -> e) -> SignalGen (Signal a) -> SignalGen (Signal b) -> SignalGen (Signal c) -> SignalGen (Signal d)
                                 -> SignalGen (Signal e)
lift4 f a b c d = f <~ a ~~ b ~~ c ~~ d

{-| Applies a function to five signals. -}
lift5 :: (a -> b -> c -> d -> e -> f) -> SignalGen (Signal a) -> SignalGen (Signal b) -> SignalGen (Signal c) -> SignalGen (Signal d)
                                      -> SignalGen (Signal e) -> SignalGen (Signal f)
lift5 f a b c d e = f <~ a ~~ b ~~ c ~~ d ~~ e

{-| Applies a function to six signals. -}
lift6 :: (a -> b -> c -> d -> e -> f -> g) -> SignalGen (Signal a) -> SignalGen (Signal b) -> SignalGen (Signal c) -> SignalGen (Signal d)
                                           -> SignalGen (Signal e) -> SignalGen (Signal f) -> SignalGen (Signal g)
lift6 f a b c d e f1 = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1

{-| Applies a function to seven signals. -}
lift7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> SignalGen (Signal a) -> SignalGen (Signal b) -> SignalGen (Signal c) -> SignalGen (Signal d)
                                                -> SignalGen (Signal e) -> SignalGen (Signal f) -> SignalGen (Signal g) -> SignalGen (Signal h)
lift7 f a b c d e f1 g = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1 ~~ g

{-| Applies a function to eight signals. -}
lift8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> SignalGen (Signal a) -> SignalGen (Signal b) -> SignalGen (Signal c) -> SignalGen (Signal d)
                                                     -> SignalGen (Signal e) -> SignalGen (Signal f) -> SignalGen (Signal g) -> SignalGen (Signal h)
                                                     -> SignalGen (Signal i)
lift8 f a b c d e f1 g h = f <~ a ~~ b ~~ c ~~ d ~~ e ~~ f1 ~~ g ~~ h

{-| An alias for 'lift'. -}
(<~) :: (a -> b) -> SignalGen (Signal a) -> SignalGen (Signal b)
(<~) = lift

infixl 4 <~

{-| Applies a function within a signal to a signal. This is a wrapper around the builtin '<*>' operator
    that automatically binds the input signal out of the signal generator.

    > render <~ Window.dimensions ~~ Window.position
 -}
(~~) :: SignalGen (Signal (a -> b)) -> SignalGen (Signal a) -> SignalGen (Signal b)
(~~) f input = do
	f1 <- f
	input1 <- input

	return $ f1 <*> input1

infixl 4 ~~

{-| Creates a past-dependent signal that depends on another signal. This is a
    wrapper around the 'transfer' function that automatically binds the input
    signal out of the signal generator. This function is useful for making a render
    function that depends on some accumulated state. -}
foldp :: (a -> b -> b) -> b -> SignalGen (Signal a) -> SignalGen (Signal b)
foldp f ini input = do
	input1 <- input

	transfer ini f input1

{-| Creates a signal that counts the amount of times it has been sampled. -}
count :: SignalGen (Signal Int)
count = stateful 0 (+ 1)

{-| Creates a signal that counts the amount of times an input signal has passed
    a predicate when sampled. -}
countIf :: (a -> Bool) -> SignalGen (Signal a) -> SignalGen (Signal Int)
countIf f = foldp (\v c -> c + fromEnum (f v)) 0
