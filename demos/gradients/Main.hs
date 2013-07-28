{-| Renders a few shapes filled with gradients.
    See http://helm-engine.org/guide/gradients/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window

{-| A list of color stops. Results in a black color at the start and white at the end. -}
stopsA :: [(Double, Color)]
stopsA = [(0, black), (1, white)]

{-| A list of color stops. This particular list results in a black color at the start,
    red at the middle and yellow at the end. -}
stopsB :: [(Double, Color)]
stopsB = [(0, black), (0.5, red), (1, yellow)]

{-| A linear gradient. Finishes at /(0, 100)/ and uses the colors defined in 'stopsB'. -}
linearGrad :: Gradient
linearGrad = linear (0, 0) (0, 100) stopsB

{-| A radial gradient. Has a radius of 64 and uses the colors defined in 'stopsA'. -}
radialGrad :: Gradient
radialGrad = radial (0, 0) 0 (0, 0) 64 stopsA

{-| The function that renders the game. It takes the window dimensions and returns an element. -}
render :: (Int, Int) -> Element
render (w, h) = collage w h [move (250, 150) $ gradient linearGrad $ rect 300 100,
                             move (500, 500) $ gradient radialGrad $ circle 64]

{-| Bootstrap the game. -}
main :: IO ()
main = run $ fmap (fmap render) Window.dimensions
