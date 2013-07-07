module FRP.Helm.Graphics (
  Element(..),
  Form(..),
  FillStyle(..),
  LineCap(..),
  LineJoin(..),
  LineStyle(..),
  defaultLine,
  solid,
  dashed,
  dotted,
  FormStyle(..),
  filled,
  outlined,
  traced,
  sprite,
  toForm,
  group,
  groupTransform,
  rotate,
  scale,
  move,
  moveX,
  moveY,
  collage,
  Path,
  path,
  segment,
  Shape,
  polygon,
  rect,
  square,
  oval,
  circle,
  ngon
) where

import FRP.Helm.Color as Color
import Graphics.Rendering.Cairo.Matrix (Matrix, identity)

data Element = CollageElement Int Int [Form] |
               ImageElement String

data Form = Form {
  theta :: Double,
  scalar :: Double,
  x :: Double,
  y :: Double,
  style :: FormStyle
}
data FillStyle = Solid Color -- Texture String | Gradient Gradient
data LineCap = Flat | Round | Padded
data LineJoin = Smooth | Sharp Double | Clipped
data LineStyle = LineStyle {
  color :: Color,
  width :: Double,
  cap :: LineCap,
  join :: LineJoin,
  dashing :: [Double],
  dashOffset :: Double
}

defaultLine :: LineStyle
defaultLine = LineStyle {
  color = Color.rgb 0 0 0,
  width = 1,
  cap = Flat,
  join = Sharp 10,
  dashing = [],
  dashOffset = 0
}

solid :: Color -> LineStyle
solid color = defaultLine { color = color }

dashed :: Color -> LineStyle
dashed color = defaultLine { color = color, dashing = [8, 4] }

dotted :: Color -> LineStyle
dotted color = defaultLine { color = color, dashing = [3, 3] }

data FormStyle = PathForm LineStyle Path |
                 ShapeForm (Either LineStyle FillStyle) Shape |
                 ImageForm Int Int (Int, Int) String |
                 ElementForm Element |
                 GroupForm Matrix [Form]

form :: FormStyle -> Form
form style = Form { theta = 0, scalar = 1, x = 0, y = 0, style = style }

fill :: FillStyle -> Shape -> Form
fill style shape = form (ShapeForm (Right style) shape)

filled :: Color -> Shape -> Form
filled color shape = fill (Solid color) shape

{-
textured :: String -> Shape -> Form

gradient :: Gradient -> 
 -}

outlined :: LineStyle -> Shape -> Form
outlined style shape = form (ShapeForm (Left style) shape)

traced :: LineStyle -> Path -> Form
traced style p = form (PathForm style p)

sprite :: Int -> Int -> (Int, Int) -> String -> Form
sprite w h pos src = form (ImageForm w h pos src)

toForm :: Element -> Form
toForm element = form (ElementForm element)

group :: [Form] -> Form
group forms = form (GroupForm identity forms)

groupTransform :: Matrix -> [Form] -> Form
groupTransform matrix forms = form (GroupForm matrix forms)

rotate :: Double -> Form -> Form
rotate t f= f { theta = t + theta f }

scale :: Double -> Form -> Form
scale n f = f { scalar = n + scalar f }

move :: (Double, Double) -> Form -> Form
move (rx, ry) f = f { x = rx + x f, y = ry + y f }

moveX :: Double -> Form -> Form
moveX x f = move (x, 0) f

moveY :: Double -> Form -> Form
moveY y f = move (0, y) f

collage :: Int -> Int -> [Form] -> Element
collage w h forms = CollageElement w h forms

type Path = [(Double, Double)]

path :: [(Double, Double)] -> Path
path points = points

segment :: (Double, Double) -> (Double, Double) -> Path
segment p1 p2 = [p1,p2]

type Shape = [(Double, Double)]

polygon :: [(Double, Double)] -> Shape
polygon points = points

rect :: Double -> Double -> Shape
rect w h = [(-hw, -hh), (-hw, hh), (hw, hh), (hw, -hh)]
  where
    hw = w / 2
    hh = h / 2

square :: Double -> Shape
square n = rect n n

oval :: Double -> Double -> Shape
oval w h = map (\i -> (hw * cos (t * i), hh * sin (t * i))) [0 .. n - 1]
  where
    n = 50
    t = 2 * pi / n
    hw = w / 2
    hh = h / 2

circle :: Double -> Shape
circle r = oval (2 * r) (2 * r)

ngon :: Int -> Double -> Shape
ngon n r = map (\i -> (r * cos (t * i), r * sin (t * i))) [0 .. fromIntegral (n - 1)]
  where 
    m = fromIntegral n
    t = 2 * pi / m
