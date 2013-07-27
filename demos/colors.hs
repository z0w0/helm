{-| Renders a colour-wheelish looking thing, with each slice being a unique pre-defined color.
    See http://helm-engine.org/guide/colors/ -}
module Colors where

import FRP.Helm
import qualified FRP.Helm.Window as Window

{-| A list of colors to show on the colour-wheel. Each colour has its own slice. -}
colors :: [Color]
colors = [red, lime, blue, yellow, cyan, magenta, maroon, navy, green, teal, purple]

{-| Calculates the point on the circumference of a specific radius at a certain angle. -}
pointOnCircum :: Double -> Double -> (Double, Double)
pointOnCircum r theta = (r * (cos theta - sin theta), r * (cos theta + sin theta))

{-| Renders a slice, using the nth color in 'colors'. -}
slice :: Int -> Form
slice n = filled color $ polygon points
  where
    color = colors !! n
    increment = 2 * pi / realToFrac (length colors)
    t1 = increment * realToFrac n
    t2 = t1 + increment
    r = 150
    points = [(0, 0), pointOnCircum r t1, pointOnCircum r t2]

{-| Renders all the slices together to form a colour-wheel, centering it on the screen. -}
render :: (Int, Int) -> Element
render (w, h) = centeredCollage w h $ map slice [0 .. length colors - 1]

{-| Bootstrap the game. -}
main :: IO ()
main = run $ fmap (fmap render) Window.dimensions
