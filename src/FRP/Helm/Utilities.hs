{-| Contains miscellaneous utility functions such as functions for working with signals and signal generators. -}
module FRP.Helm.Utilities (
  -- * Angles
  radians,
  degrees,
  turns,
  -- * Applying
  (<|),
  (|>),
  -- * Random numbers
  random,
  randomR
) where

import Control.Applicative
import Control.Monad (liftM)
import FRP.Elerea.Simple hiding (Signal)
import FRP.Helm.Sample
import FRP.Helm.Signal
import System.Random (Random, randomIO, randomRIO)

{-| Converts radians into the standard angle measurement (radians). -}
radians :: Double -> Double
radians n = n

{-| Converts degrees into the standard angle measurement (radians). -}
degrees :: Double -> Double
degrees n = n * pi / 180

{-| Converts turns into the standard angle measurement (radians).
    Turns are essentially full revolutions of the unit circle. -}
turns :: Double -> Double
turns n = 2 * pi * n

{-| Forward function application, think of it as a inverted '($)'. Provided for easy porting from Elm. -}
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

{-| Exactly the same as '($)', only there to make code using '(|>)'
    more consistent. -}
(<|) :: (a -> b) -> a -> b
(<|) = ($)

{-| Creates a signal of a random number. -}
--FIXME: This will drive the program at a stupidly fast rate
random :: Random a => Signal a
random = Signal $ effectful $ liftM Changed randomIO

{-| Creates a signal of a random number based on the given range. -}
--FIXME: This will drive the program at a stupidly fast rate
randomR :: Random a => (a, a) -> Signal a
randomR = Signal . effectful . (liftM Changed) . randomRIO
