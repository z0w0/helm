module FRP.Helm.Color where

data Color = Color { r :: !Double, g :: !Double, b :: !Double, a :: !Double }

-- |Creates an RGB color.
rgb :: Double -> Double -> Double -> Color
rgb r g b = Color r g b 1

-- |Creates an RGB color, with transparency.
rgba :: Double -> Double -> Double -> Double -> Color
rgba = Color

red :: Color
red = rgb 1 0 0

lime :: Color
lime = rgb 0 1 0

blue :: Color
blue = rgb 0 0 1

yellow :: Color
yellow = rgb 1 1 0

cyan :: Color
cyan = rgb 0 1 1

magenta :: Color
magenta = rgb 1 0 1

black :: Color
black = rgb 0 0 0

white :: Color
white = rgb 1 1 1

gray :: Color
gray = rgb 0.5 0.5 0.5

grey :: Color
grey = gray

maroon :: Color
maroon = rgb 0.5 0 0

navy :: Color
navy = rgb 0 0 0.5

green :: Color
green = rgb 0 0.5 0

teal :: Color
teal = rgb 0 0.5 0.5

purple :: Color
purple = rgb 0.5 0 0.5

violet :: Color
violet = rgb 0.923 0.508 0.923

forestGreen :: Color
forestGreen = rgb 0.133 0.543 0.133

{-

complement :: Color -> Color

hsva :: Double -> Double -> Double -> Double -> Color

hsv :: Double -> Double -> Double -> Color
-}

{-
data Gradient = Linear (Double, Double) (Double, Double) [(Double, Color)] |
                Radial (Double, Double) Double (Double, Double) Double [(Double, Color)]


-- |Creates a linear gradient. Takes an 
linear :: (Double, Double) -> (Double, Double) -> [(Double, Color)] -> Gradient
linear = Linear

-- |Creates a radial gradient.
radial :: (Double, Double) -> Double -> (Double, Double) -> Double -> [(Double, Color)] -> Gradient
radial = Radial
-}
