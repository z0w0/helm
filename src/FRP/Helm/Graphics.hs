{-| Contains all the data structures and functions for composing
    and rendering graphics. -}
module FRP.Helm.Graphics (
  -- * Types
  Element(..),
  FontWeight(..),
  FontStyle(..),
  Text(..),
  Form(..),
  FormStyle(..),
  FillStyle(..),
  LineCap(..),
  LineJoin(..),
  LineStyle(..),
  Path,
  Shape(..),
  -- * Elements
  image,
  fittedImage,
  croppedImage,
  collage,
  centeredCollage,
  fixedCollage,
  -- * Styles & Forms
  defaultLine,
  solid,
  dashed,
  dotted,
  filled,
  textured,
  gradient,
  outlined,
  traced,
  sprite,
  toForm,
  blank,
  -- * Grouping
  group,
  groupTransform,
  -- * Transforming
  rotate,
  scale,
  move,
  moveX,
  moveY,
  -- * Paths
  path,
  segment,
  -- * Shapes
  polygon,
  rect,
  square,
  oval,
  circle,
  ngon
) where

import FRP.Helm.Color (Color, black, Gradient)
import Graphics.Rendering.Cairo.Matrix (Matrix)

{-| A data structure describing the weight of a piece of font. -}
data FontWeight = LightWeight |
                  NormalWeight |
                  BoldWeight deriving (Show, Eq, Ord, Enum, Read)

{-| A data structure describing the style of of a piece of font. -}
data FontStyle = NormalStyle |
                 ObliqueStyle |
                 ItalicStyle deriving (Show, Eq, Ord, Enum, Read)

{-| A data structure describing a piece of formatted text. -}
data Text = Text {
  textUTF8 :: String,
  textColor :: Color,
  textTypeface :: String,
  textHeight :: Double,
  textWeight :: FontWeight,
  textStyle :: FontStyle
} deriving (Show, Eq)

{-| A data structure describing something that can be rendered
    to the screen. Elements are the most important structure
    in Helm. Games essentially feed the engine a stream
    of elements which are then rendered directly to the screen.
    The usual way to render art in a Helm game is to call
    off to the 'collage' function, which essentially
    renders a collection of forms together. -}
data Element = CollageElement Int Int (Maybe (Double, Double)) [Form] |
               ImageElement (Int, Int) Int Int FilePath Bool |
               TextElement Text deriving (Show, Eq)

{-| Create an element from an image with a given width, height and image file path.
    If the image dimensions are not the same as given, then it will stretch/shrink to fit.
    Only PNG files are supported currently. -}
image :: Int -> Int -> FilePath -> Element
image w h src = ImageElement (0, 0) w h src True

{-| Create an element from an image with a given width, height and image file path.
    If the image dimensions are not the same as given, then it will only use the relevant pixels
    (i.e. cut out the given dimensions instead of scaling). If the given dimensions are bigger than
    the actual image, than irrelevant pixels are ignored. -}
fittedImage :: Int -> Int -> FilePath -> Element
fittedImage w h src = ImageElement (0, 0) w h src False

{-| Create an element from an image by cropping it with a certain position, width, height
    and image file path. This can be used to divide a single image up into smaller ones. -}
croppedImage :: (Int, Int) -> Int -> Int -> FilePath -> Element
croppedImage pos w h src = ImageElement pos w h src False

{-| A data structure describing a form. A form is essentially a notion of a transformed
    graphic, whether it be an element or shape. See 'FormStyle' for an insight
    into what sort of graphics can be wrapped in a form. -}
data Form = Form {
  formTheta :: Double,
  formScale :: Double,
  formX :: Double,
  formY :: Double,
  formStyle :: FormStyle
} deriving (Show, Eq)

{-| A data structure describing how a shape or path looks when filled. -}
data FillStyle = Solid Color | Texture String | Gradient Gradient deriving (Show, Eq, Ord, Read)

{-| A data structure describing the shape of the ends of a line. -}
data LineCap = FlatCap | RoundCap | PaddedCap deriving (Show, Eq, Enum, Ord, Read)

{-| A data structure describing the shape of the join of a line, i.e.
    where separate line segments join. The 'Sharp' variant takes
    an argument to limit the length of the join. -}
data LineJoin = SmoothJoin | SharpJoin Double | ClippedJoin deriving (Show, Eq, Ord, Read)

{-| A data structure describing how a shape or path looks when stroked. -}
data LineStyle = LineStyle {
  lineColor :: Color,
  lineWidth :: Double,
  lineCap :: LineCap,
  lineJoin :: LineJoin,
  lineDashing :: [Double],
  lineDashOffset :: Double
} deriving (Show, Eq)

{-| Creates the default line style. By default, the line is black with a width of 1,
    flat caps and regular sharp joints. -}
defaultLine :: LineStyle
defaultLine = LineStyle {
  lineColor = black,
  lineWidth = 1,
  lineCap = FlatCap,
  lineJoin = SharpJoin 10,
  lineDashing = [],
  lineDashOffset = 0
}

{-| Create a solid line style with a color. -}
solid :: Color -> LineStyle
solid color = defaultLine { lineColor = color }

{-| Create a dashed line style with a color. -}
dashed :: Color -> LineStyle
dashed color = defaultLine { lineColor = color, lineDashing = [8, 4] }

{-| Create a dotted line style with a color. -}
dotted :: Color -> LineStyle
dotted color = defaultLine { lineColor = color, lineDashing = [3, 3] }

{-| A data structure describing a few ways that graphics that can be wrapped in a form
    and hence transformed. -}
data FormStyle = PathForm LineStyle Path |
                 ShapeForm (Either LineStyle FillStyle) Shape |
                 ElementForm Element |
                 GroupForm (Maybe Matrix) [Form] deriving (Show, Eq)

{-| Utility function for creating a form. -}
form :: FormStyle -> Form
form style = Form { formTheta = 0, formScale = 1, formX = 0, formY = 0, formStyle = style }

{-| Utility function for creating a filled form from a fill style and shape. -}
fill :: FillStyle -> Shape -> Form
fill style shape = form (ShapeForm (Right style) shape)

{-| Creates a form from a shape by filling it with a specific color. -}
filled :: Color -> Shape -> Form
filled color = fill (Solid color)

{-| Creates a form from a shape with a tiled texture and image file path. -}
textured :: String -> Shape -> Form
textured src = fill (Texture src)

{-| Creates a form from a shape filled with a gradient. -}
gradient :: Gradient -> Shape -> Form
gradient grad = fill (Gradient grad)

{-| Creates a form from a shape by outlining it with a specific line style. -}
outlined :: LineStyle -> Shape -> Form
outlined style shape = form (ShapeForm (Left style) shape)

{-| Creates a form from a path by tracing it with a specific line style. -}
traced :: LineStyle -> Path -> Form
traced style p = form (PathForm style p)

{-| Creates a form from a image file path with additional position, width and height arguments.
    Allows you to splice smaller parts from a single image. -}
sprite :: Int -> Int -> (Int, Int) -> FilePath -> Form
sprite w h pos src = form (ElementForm (ImageElement pos w h src False))

{-| Creates a form from an element. -}
toForm :: Element -> Form
toForm element = form (ElementForm element)

{-| Creates a empty form, useful for having forms rendered only at some state. -}
blank :: Form
blank = group []

{-| Groups a collection of forms into a single one. -}
group :: [Form] -> Form
group forms = form (GroupForm Nothing forms)

{-| Groups a collection of forms into a single one, also applying a matrix transformation. -}
groupTransform :: Matrix -> [Form] -> Form
groupTransform matrix forms = form (GroupForm (Just matrix) forms)

{-| Rotates a form by an amount (in radians). -}
rotate :: Double -> Form -> Form
rotate t f = f { formTheta = t + formTheta f }

{-| Scales a form by an amount, e.g. scaling by /2.0/ will double the size. -}
scale :: Double -> Form -> Form
scale n f = f { formScale = n * formScale f }

{-| Moves a form relative to its current position. -}
move :: (Double, Double) -> Form -> Form
move (rx, ry) f = f { formX = rx + formX f, formY = ry + formY f }

{-| Moves a form's x-coordinate relative to its current position. -}
moveX :: Double -> Form -> Form
moveX x = move (x, 0)

{-| Moves a form's y-coordinate relative to its current position. -}
moveY :: Double -> Form -> Form
moveY y = move (0, y)

{-| Create an element from a collection of forms, with width and height arguments.
    All forms are centered and clipped within the supplied dimensions.
    It is generally used to directly render a collection of forms.

    > collage 800 600 [move (100, 100) $ filled red $ square 100,
    >                  move (100, 100) $ outlined (solid white) $ circle 50]
 -}
collage :: Int -> Int -> [Form] -> Element
collage w h = CollageElement w h Nothing

{-| Like 'collage', but it centers the forms within the supplied dimensions. -}
centeredCollage :: Int -> Int -> [Form] -> Element
centeredCollage w h = CollageElement w h (Just (realToFrac w / 2, realToFrac h / 2))

{-| Like 'centeredCollage', but it centers the forms around a specific point. -}
fixedCollage :: Int -> Int -> (Double, Double) -> [Form] -> Element
fixedCollage w h (x, y) = CollageElement w h (Just (realToFrac w / 2 - x, realToFrac h / 2 - y))

{-| A data type made up a collection of points that form a path when joined. -}
type Path = [(Double, Double)]

{-| Creates a path for a collection of points. -}
path :: [(Double, Double)] -> Path
path points = points

{-| Creates a path from a line segment, i.e. a start and end point. -}
segment :: (Double, Double) -> (Double, Double) -> Path
segment p1 p2 = [p1, p2]

{-| A data structure describing a some sort of graphically representable object,
    such as a polygon formed from a list of points or a rectangle. -}
data Shape = PolygonShape Path |
             RectangleShape (Double, Double) |
             ArcShape (Double, Double) Double Double Double (Double, Double) deriving (Show, Eq, Ord, Read)

{-| Creates a shape from a path (a list of points). -}
polygon :: Path -> Shape
polygon = PolygonShape

{-| Creates a rectangular shape with a width and height. -}
rect :: Double -> Double -> Shape
rect w h = RectangleShape (w, h)

{-| Creates a square shape with a side length. -}
square :: Double -> Shape
square n = rect n n

{-| Creates an oval shape with a width and height. -}
oval :: Double -> Double -> Shape
oval w h = ArcShape (0, 0) 0 (2 * pi) 1 (w / 2, h / 2)

{-| Creates a circle shape with a radius. -}
circle :: Double -> Shape
circle r = ArcShape (0, 0) 0 (2 * pi) r (1, 1)

{-| Creates a generic n-sided polygon (e.g. octagon, pentagon, etc) with
    an amount of sides and radius. -}
ngon :: Int -> Double -> Shape
ngon n r = PolygonShape (map (\i -> (r * cos (t * i), r * sin (t * i))) [0 .. fromIntegral (n - 1)])
  where 
    m = fromIntegral n
    t = 2 * pi / m
