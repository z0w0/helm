{-# LANGUAGE DeriveGeneric #-}
-- | Contains all data structures and functions for composing colors.
module Helm.Color (
  -- * Types
  Color(..),
  Gradient(..),
  -- * Composing
  rgba,
  rgb,
  hsva,
  hsv,
  blend,
  complement,
  linear,
  radial
) where

import GHC.Generics

-- | Represents a color. It is represented interally as an RGBA
-- color, but the utility functions 'hsva', 'hsv', etc. can be used to convert
-- from other popular formats to this structure.
data Color = Color !Double !Double !Double !Double deriving (Show, Eq, Ord, Read, Generic)

-- | Create an RGB color.
rgb :: Double -> Double -> Double -> Color
rgb r g b = rgba r g b 1

-- | Create an RGBA color.
rgba :: Double -> Double -> Double -> Double -> Color
rgba r g b a
  | r < 0 || r > 1 ||
    g < 0 || g > 1 ||
    b < 0 || b > 1 ||
    a < 0 || a > 1 = error "Helm.Color.rgba: color components must be between 0 and 1"
  | otherwise = Color r g b a

-- | Blends colors together by averaging out their color components.
blend :: [Color] -> Color
blend colors =
  (\(Color r g b a) -> Color (r / denom) (g / denom) (b / denom) (a / denom)) $ foldl blend' black colors

  where
    black = rgb 0 0 0
    denom = fromIntegral $ length colors

-- | Adds colors together.
blend' :: Color -> Color -> Color
blend' (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) = Color (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)

-- | Calculate the complementary color for a color provided color.
-- This is useful for outlining a filled shape in a color clearly
-- distinguishable from the fill color.
complement :: Color -> Color
complement (Color r g b a) = hsva (fromIntegral ((round (h + 180) :: Int) `mod` 360)) (s / mx) mx a
  where
    mx = r `max` g `max` b
    mn = r `min` g `min` b
    s = mx - mn
    h | mx == r = (g - b) / s * 60
      | mx == g = (b - r) / s * 60 + 120
      | mx == b = (r - g) / s * 60 + 240
      | otherwise = undefined

-- | Create an RGBA color from HSVA values.
hsva :: Double -> Double -> Double -> Double -> Color
hsva h s v a
  | h'' == 0 = rgba v t p a
  | h'' == 1 = rgba q v p a
  | h'' == 2 = rgba p v t a
  | h'' == 3 = rgba p q v a
  | h'' == 4 = rgba t p v a
  | h'' == 5 = rgba v p q a
  | otherwise = undefined

  where
    h' = h / 60
    h'' = floor h' `mod` 6 :: Int
    f = h' - fromIntegral h''
    p = v * (1 - s)
    q = v * (1 - f * s)
    t = v * (1 - (1 - f) * s)

-- | Create an RGB color from HSV values.
hsv :: Double -> Double -> Double -> Color
hsv h s v = hsva h s v 1

-- | Represents a gradient.
--
-- Helm supports radial and linear gradients.
-- Radial gradients are based on a set of colors transitioned
-- over certain radii in an arc pattern. Linear gradients are a set of colors
-- transitioned in a straight line.
data Gradient
  = Linear !(Double, Double) !(Double, Double) ![(Double, Color)] -- ^ A linear gradient.
  | Radial !(Double, Double) !Double !(Double, Double) !Double ![(Double, Color)] -- ^ A radial gradient.
  deriving (Show, Eq, Ord, Read)

-- | Creates a linear gradient. Takes a starting position, ending position and a list
-- of color stops (which are colors combined with a floating value between /0.0/ and /1.0/
-- that describes at what step along the line between the starting position
-- and ending position the paired color should be transitioned to).
--
-- > linear (0, 0) (100, 100) [(0, black), (1, white)]
--
-- The above example creates a gradient that starts at /(0, 0)/
-- and ends at /(100, 100)/. In other words, it's a diagonal gradient, transitioning from the top-left
-- to the bottom-right. The provided color stops result in the gradient transitioning from
-- black to white.
linear :: (Double, Double) -> (Double, Double) -> [(Double, Color)] -> Gradient
linear = Linear

-- | Creates a radial gradient. Takes a starting position and radius, ending position and radius
-- and a list of color stops. See the document for 'linear' for more information on color stops.
radial :: (Double, Double) -> Double -> (Double, Double) -> Double -> [(Double, Color)] -> Gradient
radial = Radial
