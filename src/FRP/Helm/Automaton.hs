{-| Contains all data structures and functions for composing, calculating and creating automatons. -}
module FRP.Helm.Automaton (
  -- * Types
  Automaton(..),
  -- * Composing
  pure,
  stateful,
  combine,
  -- * Computing
  step,
  run,
  counter
) where

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))
import FRP.Elerea.Simple (Signal, SignalGen, transfer)

{-| A data structure describing an automaton.
    An automaton is essentially a high-level way to package piped behavior
    between an input signal and an output signal. Automatons can also
    be composed, allowing you to connect one automaton to another
    and pipe data between them. Automatons are an easy and powerful way
    to create composable dynamic behavior, like animation systems. -}
data Automaton a b = Step (a -> (Automaton a b, b))

instance Category Automaton where
  id = Step (\a -> (id, a))
  (Step f) . (Step g) = Step (\a -> let (g', b) = g a
                                        (f', c) = f b in (f' . g', c))

instance Arrow Automaton where
  arr = pure
  first (Step f) = Step (\(b, d) -> let (f', c) = f b in (first f', (c, d)))

{-| Creates a pure automaton that has no accumulated state. It applies input to
    a function at each step. -}
pure :: (a -> b) -> Automaton a b
pure f = Step (\x -> (pure f, f x))

{-| Creates an automaton that has an initial and accumulated state. It applies
    input and the last state to a function at each step. -}
stateful :: b -> (a -> b -> b) -> Automaton a b
stateful state f = Step (\x -> let state' = f x state in (stateful state' f, state'))

{-| Steps an automaton forward, returning the next automaton to step
    and output of the step in a tuple. -}
step :: a -> Automaton a b -> (Automaton a b, b)
step auto (Step f) = f auto

{-| Combines a list of automatons that take some input
    and turns it into an automaton that takes
    the same input and outputs a list of all outputs
    from each separate automaton. -}
combine :: [Automaton a b] -> Automaton a [b]
combine autos =
  Step (\a -> let (autos', bs) = unzip $ map (step a) autos
              in  (combine autos', bs))

{-| A useful automaton that outputs the amount of times it has been stepped,
    discarding its input value. -}
counter :: Automaton a Int
counter = stateful 0 (\_ c -> c + 1)

{-| Runs an automaton with an initial output value and input signal generator
    and creates an output signal generator that contains a signal that can be
    sampled for the output value. -}
run :: Automaton a b -> b -> SignalGen (Signal a) -> SignalGen (Signal b)
run auto ini feeder = do
  food <- feeder >>= transfer (auto, ini) (\a (Step f, _) -> f a)

  return $ fmap snd food
