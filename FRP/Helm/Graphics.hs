module FRP.Helm.Graphics (
  Element(..),
  image,
  croppedImage,
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
  textured,
  gradient,
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
               ImageElement (Int, Int) Int Int FilePath Bool

{-| Create an element from an image with a given width, height and image file path.
    If the image dimensions are not the same as given, then it will stretch/shrink to fit.
    Only PNG files are supported currently. -}
image :: Int -> Int -> FilePath -> Element
image w h src = ImageElement (0, 0) w h src True

{- TODO:
{-| Create an element from an image with a given width, height and image file path.
    If the image dimensions are not the same as given, then it will only use the relevant pixels
    (i.e. cut out the given width instead of scaling). If the given dimensions are bigger than
    the actual image, than irrelevant pixels are ignored. -}
fittedImage :: Int -> Int -> FilePath -> Element
fittedImage w h src = ImageElement (0, 0) w h src False -}

{-| Create an element from an image by cropping it with a certain position, width, height
    and image file path. This can be used to divide a single image up into smaller ones. -}
croppedImage :: (Int, Int) -> Int -> Int -> FilePath -> Element
croppedImage pos w h src = ImageElement pos w h src False

data Form = Form {
  theta :: Double,
  scalar :: Double,
  x :: Double,
  y :: Double,
  style :: FormStyle
}
data FillStyle = Solid Color | Texture String | Gradient Gradient
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

{-| Creates the default line style. By default, the line is black with a width of 1,
    flat caps and regular sharp joints. -}
defaultLine :: LineStyle
defaultLine = LineStyle {
  color = Color.black,
  width = 1,
  cap = Flat,
  join = Sharp 10,
  dashing = [],
  dashOffset = 0
}

-- |Create a solid line style with a color.
solid :: Color -> LineStyle
solid color = defaultLine { color = color }

-- |Create a dashed line style with a color.
dashed :: Color -> LineStyle
dashed color = defaultLine { color = color, dashing = [8, 4] }

-- |Create a dotted line style with a color.
dotted :: Color -> LineStyle
dotted color = defaultLine { color = color, dashing = [3, 3] }

data FormStyle = PathForm LineStyle Path |
                 ShapeForm (Either LineStyle FillStyle) Shape |
                 ElementForm Element |
                 GroupForm Matrix [Form]

form :: FormStyle -> Form
form style = Form { theta = 0, scalar = 1, x = 0, y = 0, style = style }

fill :: FillStyle -> Shape -> Form
fill style shape = form (ShapeForm (Right style) shape)

-- |Creates a form from a shape by filling it with a specific color.
filled :: Color -> Shape -> Form
filled color shape = fill (Solid color) shape

-- |Creates a form from a shape with a tiled texture and image file path.
textured :: String -> Shape -> Form
textured src shape = fill (Texture src) shape

-- |Creates a form from a shape filled with a gradient.
gradient :: Gradient -> Shape -> Form
gradient grad shape = fill (Gradient grad) shape

-- |Creates a form from a shape by outlining it with a specific line style.
outlined :: LineStyle -> Shape -> Form
outlined style shape = form (ShapeForm (Left style) shape)

-- |Creates a form from a path by tracing it with a specific line style.
traced :: LineStyle -> Path -> Form
traced style p = form (PathForm style p)

{-| Creates a form from a image file path with additional position, width and height arguments.
    Allows you to splice smaller parts from a single image. -}
sprite :: Int -> Int -> (Int, Int) -> FilePath -> Form
sprite w h pos src = form (ElementForm (ImageElement pos w h src False))

-- |Creates a form from an element.
toForm :: Element -> Form
toForm element = form (ElementForm element)

-- |Groups a collection of forms into a single one.
group :: [Form] -> Form
group forms = form (GroupForm identity forms)

-- |Groups a collection of forms into a single one, also applying a matrix transformation.
groupTransform :: Matrix -> [Form] -> Form
groupTransform matrix forms = form (GroupForm matrix forms)

-- |Rotates a form by an amount (in radians).
rotate :: Double -> Form -> Form
rotate t f = f { theta = t + theta f }

-- |Scales a form by an amount, e.g. scaling by 2 will double the size.
scale :: Double -> Form -> Form
scale n f = f { scalar = n + scalar f }

-- |Moves a form relative to its current position.
move :: (Double, Double) -> Form -> Form
move (rx, ry) f = f { x = rx + x f, y = ry + y f }

-- |Moves a form's x-coordinate relative to its current position.
moveX :: Double -> Form -> Form
moveX x f = move (x, 0) f

-- |Moves a form's y-coordinate relative to its current position.
moveY :: Double -> Form -> Form
moveY y f = move (0, y) f

{-| Create an element from a collection of forms.
    Can be used to directly render a collection of forms. -}
collage :: Int -> Int -> [Form] -> Element
collage w h forms = CollageElement w h forms

type Path = [(Double, Double)]

-- |Creates a path for a collection of points.
path :: [(Double, Double)] -> Path
path points = points

-- |Creates a path from a line segment, i.e. a start and end point.
segment :: (Double, Double) -> (Double, Double) -> Path
segment p1 p2 = [p1,p2]

type Shape = [(Double, Double)]

-- |Creates a shape from a set of points.
polygon :: [(Double, Double)] -> Shape
polygon points = points

-- |Creates a rectangular shape with a specific width and height.
rect :: Double -> Double -> Shape
rect w h = [(-hw, -hh), (-hw, hh), (hw, hh), (hw, -hh)]
  where
    hw = w / 2
    hh = h / 2

-- |Creates a square shape with a specific side length.
square :: Double -> Shape
square n = rect n n

-- |Creates an oval shape with a specific width and height.
oval :: Double -> Double -> Shape
oval w h = map (\i -> (hw * cos (t * i), hh * sin (t * i))) [0 .. n - 1]
  where
    n = 50
    t = 2 * pi / n
    hw = w / 2
    hh = h / 2

-- |Creates an oval shape with a specific radius.
{- TODO: Make a separate variant of a shape that is based on an arc.
   as this sort of thing is going to be pretty unaccurate and slower. -}
circle :: Double -> Shape
circle r = oval (2 * r) (2 * r)

-- |Creates a generic n-sided polygon (e.g. octagon) with a specific amount of sides and radius.
ngon :: Int -> Double -> Shape
ngon n r = map (\i -> (r * cos (t * i), r * sin (t * i))) [0 .. fromIntegral (n - 1)]
  where 
    m = fromIntegral n
    t = 2 * pi / m
