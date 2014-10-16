{-| Contains miscellaneous utility functions such as functions for working with signals and signal generators. -}
module FRP.Helm.Utilities (
  -- * Angles
  radians,
  degrees,
  turns,
  -- * Applying
  (<|),
  (|>),
) where

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
