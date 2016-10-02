-- | Contains all the types and functions for composing
-- and rendering 2D graphics.
module Helm.Graphics2D
  (
    -- * Types
    Collage(..)
  , Form(..)
  , FormStyle(..)
  , FillStyle(..)
  , LineCap(..)
  , LineJoin(..)
  , LineStyle(..)
  , Path(..)
  , Shape(..)
  , ShapeStyle(..)
  , Transform(..)
  , Text(..)
  -- * Collages
  , collage
  , clip
  , center
  , toForm
  -- * Styles & Forms
  , defaultLine
  , solid
  , dashed
  , dotted
  , filled
  , textured
  , gradient
  , outlined
  , traced
  , image
  , fittedImage
  , croppedImage
  , blank
  , alpha
  , text
  -- * Grouping
  , group
  , groupTransform
  -- * Transforming
  , rotate
  , scale
  , move
  -- * Paths
  , path
  -- * Shapes
  , polygon
  , rect
  , square
  , oval
  , circle
  , ngon
  ) where

import Linear.V2 (V2(V2))

import Helm.Asset (Image)
import Helm.Color (Color, rgb, Gradient)
import Helm.Graphics2D.Text (Text(..))
import Helm.Graphics2D.Transform (Transform(..), identity)

-- | Represents a collection of forms, which in turn are rendereable
-- shapes and lines. In Helm, the collage is the main structure
-- representing 2D graphics and is passed directly to the engine
-- to be rendered by your view function. It's best to think of a collage
-- as a fancy version of a game screen, with the difference being that the
-- collage itself knows nothing about the window state. It only knows
-- what will be rendered to the screen (which in this case, is a series of forms)
-- and the order in which they will be rendered.
data Collage e = Collage
  { collageDims :: Maybe (V2 Double)    -- ^ The optional dimensions of the collage. It will be clipped to these dims.
  , collageForms :: [Form e]            -- ^ The collection of forms under the collage.
  , collageCenter :: Maybe (V2 Double)  -- ^ The optional center of the collage.
  }

-- | Create a collage from a list of forms.
-- By default, the collage will not be clipped
-- and will not be centered. The origin point of the contained
-- forms will be the top-left of the collage (which in the case of rendering
-- a collage to the screen, is coincidently the top-left of the game window).
-- See 'center' and 'clip'.
collage :: [Form e] -> Collage e
collage forms = Collage
  { collageDims = Nothing
  , collageForms = forms
  , collageCenter = Nothing
  }

-- | Center a collage around a fixed point. This is useful to implement
-- 2D game cameras - usually, you have the center of the screen
-- at the position of the game camera (which in a 2D platformer,
-- is usually your game character). Note that this will center
-- the forms themselves, i.e. their original point will change from being
-- the top left of the collage to
center ::
     V2 Double  -- ^ The position to center the collage at.
  -> Collage e  -- ^ The source collage.
  -> Collage e  -- ^ The centered collage.
center pos col = col { collageCenter = Just pos }

-- | Clip a collage by provided dimensions. Note that by default,
-- a collage will not be clipped and anything beyond the window dimensions
-- will still technically be rendered (although obviously it will not appear
-- on the game screen). By composing a collage with this function,
-- when the collage is rendered its contents will be clipped by these dimensions.
-- Not only will this generally speed up performance, it can be used for certain
-- cases where you don't want the forms in the collage to spill over
-- to other collages near it (but that's a very rare use-case).
--
-- Something to note, that this is absolutely not an ensurance that your 2D graphics
-- will be rendered quickly if you're doing a lot of graphics work. The clip merely
-- prevents things being drawn outside the dimensions, which in most cases will
-- indeed speed up the performance, but it is down to the engine implementation for how much
-- this actually helps.
--
-- In that sense, it's up to the library user to make sure they're not rendering huge amounts
-- of forms that aren't even in the screen's bounds.
clip ::
     V2 Double  -- ^ The dimensions to clip the collage with.
  -> Collage e  -- ^ The source collage.
  -> Collage e  -- ^ The clipped collage.
clip dims col = col { collageDims = Just dims }

-- | Create a form from a collage. This might seem a little strange (as
-- a collage is generally what you provide to the engine to render the 2D graphics)
-- but by allowing this functionality, you can compose collages from other collages.
toForm :: Collage e -> Form e
toForm = defaultForm . CollageForm

-- | Represents the styles of forms available. The form style holds data specific
-- to a variation of form, and the 'Form' is instead a general version of this
-- with positioning information, rotation, scale, etc.
data FormStyle e
  = PathForm LineStyle Path                           -- ^ A form composed of a path
  | ShapeForm (ShapeStyle e) Shape                    -- ^ A form composed of a shape.
  | TextForm Text                                     -- ^ A form composed of a piece of text, including string and style info.
  | ImageForm (Image e) (V2 Double) (V2 Double) Bool  -- ^ A form composed of an image
  | GroupForm Transform [Form e]                      -- ^ A form composed of a group of forms, with a transformation.
  | CollageForm (Collage e)                           -- ^ A form composed of a collage (which in turn is a collection of forms).

-- | Represents something that can be rendered to the screen (
-- contained under a collage). There are many different types of forms, which can be composed
-- below but are generally represented by the 'FormStyle' type.
--
-- A form might be an image, or a rectangle, or a circle, or even a collection
-- of forms (which in turn can be those same things).
data Form e = Form
  { formTheta :: Double       -- ^ The rotation of the form (in radians).
  , formScale :: Double       -- ^ The scale factor of the form.
  , formPos :: V2 Double      -- ^ The position of the form. This will be rendered relative to the collage origin.
  , formAlpha :: Double       -- ^ The alpha channel of the form.
  , formStyle :: FormStyle e  -- ^ The style of form.
  }

-- | Represents the style of shape filling available.
data FillStyle e
  = Solid Color        -- ^ The shape will be filled with a solid color.
  | Texture (Image e)  -- ^ The shape will be filled with a texture (a.k.a. image).
  | Gradient Gradient  -- ^ The shape will be filled with a gradient (which can be linear or radial).

-- | Represents the shape of the ends of a line.
data LineCap
  = FlatCap
  | RoundCap
  | PaddedCap
    deriving (Show, Eq, Ord, Read)

-- | Represents the shape of the joints between line segments.
data LineJoin
  = SmoothJoin
  | SharpJoin Double
  | ClippedJoin
    deriving (Show, Eq, Ord, Read)

-- | Represents the style used for drawing lines. It's best
-- to use 'defaultLine' and then only change the fields
-- you need to.
data LineStyle = LineStyle
  { lineColor :: Color
  , lineWidth :: Double
  , lineCap :: LineCap
  , lineJoin :: LineJoin
  , lineDashing :: [Double]
  , lineDashOffset :: Double
  } deriving (Show, Eq)

-- | Represents a series of 2D points which will be drawn in sequence.
-- Like a 'Shape', a path on its own holds no styling information.
data Path = Path [V2 Double] deriving (Show, Eq, Ord, Read)

-- | Create a path from a sequence of points (represented as 2D vectors).
path :: [V2 Double] -> Path
path = Path

-- | Represents a collection of points that when drawn in order
-- will result in a closed polygon. They have no style information -
-- rather, you compose shapes into a form with fill or line style
-- and that affects their appearance.
--
-- Note that realistically, a shape could be represented as
-- a path only. However, we add extra variants here as drawing backends
-- usually provide optimized forms of drawing circles (and perhaps rectangles,
-- although that is less likely) hence it's better to fall to those
-- if our shape is circular.
data Shape
  = PolygonShape Path
  | RectangleShape (V2 Double)
  | ArcShape (V2 Double) Double Double Double (V2 Double)
    deriving (Show, Eq, Ord, Read)

-- | Create an arbitary-sided polygon from a path.
-- The points provided should refer to each corner of the -gon,
-- however the points do not need to loop around (i.e. the final point
-- will automatically connect to the first point).
polygon :: Path -> Shape
polygon = PolygonShape

-- | Create a rectangular shape from a 2D vector, with
-- x and y representing width and height, respectively.
rect :: V2 Double -> Shape
rect = RectangleShape

-- | Create a square shape with a side length.
square :: Double -> Shape
square n = rect (V2 n n)

-- | Create an oval shape with a width and height.
oval :: Double -> Double -> Shape
oval w h = ArcShape (V2 0 0) 0 (2 * pi) 1 (V2 (w / 2) (h / 2))

-- | Create a circle shape with a radius.
circle :: Double -> Shape
circle r = ArcShape (V2 0 0) 0 (2 * pi) r (V2 1 1)

-- | Create a generic n-sided polygon (e.g. octagon, pentagon, etc) with
-- a side count and radius.
ngon :: Int -> Double -> Shape
ngon n r = polygon $ path $ map point series
  where
    point i = V2 (r * cos (t * i)) (r * sin (t * i))
    series = [0 .. fromIntegral (n - 1)]
    m = fromIntegral n
    t = 2 * pi / m

-- | Create the default line style. By default, the line is black with a width of 1,
-- flat caps and regular sharp joints.
defaultLine :: LineStyle
defaultLine = LineStyle
  { lineColor = rgb 0 0 0
  , lineWidth = 1
  , lineCap = FlatCap
  , lineJoin = SharpJoin 10
  , lineDashing = []
  , lineDashOffset = 0
  }

-- | Create a initial form from a specific form style.
-- The form will be at the origin point (0, 0).
defaultForm :: FormStyle e -> Form e
defaultForm style = Form
  { formTheta = 0
  , formScale = 1
  , formPos = V2 0 0
  , formAlpha = 1
  , formStyle = style
  }

-- | Represents the style used for drawing a shape.
data ShapeStyle e
  = OutlinedShape LineStyle    -- ^ Stroke/outline the shape, with a specific line style.
  | FilledShape (FillStyle e)  -- ^ Fill the shape, with a specific fill style.

-- | Create a solid line style with a color.
solid :: Color -> LineStyle
solid color = defaultLine { lineColor = color }

-- | Create a dashed line style with a color.
dashed :: Color -> LineStyle
dashed color = defaultLine { lineColor = color, lineDashing = [8, 4] }

-- | Create a dotted line style with a color.
dotted :: Color -> LineStyle
dotted color = defaultLine { lineColor = color, lineDashing = [3, 3] }

-- | Fill a shape with a specific fill style.
fill :: FillStyle e -> Shape -> Form e
fill style shape = defaultForm (ShapeForm (FilledShape style) shape)

-- | Fill a shape with a color.
filled :: Color -> Shape -> Form e
filled color = fill (Solid color)

-- | Fill a shape with a texture. The texture should
-- be an image loaded by the engine.
textured :: Image e -> Shape -> Form e
textured img = fill (Texture img)

-- | Fill a shape with a gradient (either 'linear' or 'radial').
gradient :: Gradient -> Shape -> Form e
gradient grad = fill (Gradient grad)

-- | Create a form from a shape by outlining it with a specific line style.
outlined :: LineStyle -> Shape -> Form e
outlined style shape = defaultForm (ShapeForm (OutlinedShape style) shape)

-- | Create a form from a path by tracing it with a specific line style.
traced :: LineStyle -> Path -> Form e
traced style p = defaultForm (PathForm style p)

-- | Create an empty form, useful for having forms rendered only at some state.
blank :: Form e
blank = group []

-- | Create a form from an image. If the image dimensions are not the
-- same as provided, then it will stretch/shrink to fit.
image :: V2 Double -> Image e -> Form e
image dims img = defaultForm $ ImageForm img (V2 0 0) dims True

-- | Create a form from an image with a 2D vector describing its dimensions.
-- If the image dimensions are not the same as given, then it will only use the relevant pixels
-- (i.e. cut out the given dimensions instead of scaling). If the given dimensions are bigger than
-- the actual image, than irrelevant pixels are ignored.
fittedImage :: V2 Double -> Image e -> Form e
fittedImage dims img = defaultForm $ ImageForm img (V2 0 0) dims False

-- | Create a form from an image by cropping it with a certain position, width, height
-- and image file path. This can be used to divide a single image up into smaller ones (
-- for example, drawing a single sprite from a sprite sheet).
croppedImage :: V2 Double -> V2 Double -> Image e -> Form e
croppedImage pos dims img = defaultForm $ ImageForm img pos dims False

-- | Group a list of forms into one. They will be drawn in their
-- sequential order within the list.
group :: [Form e] -> Form e
group forms = defaultForm (GroupForm identity forms)

-- | Group a list of forms into one, while also applying a matrix
-- transformation.
groupTransform :: Transform -> [Form e] -> Form e
groupTransform matrix forms = defaultForm (GroupForm matrix forms)

-- | Move a form by a given 2D vector. The movement is relative,
-- i.e. the translation vector provided will be added to the form's
-- current position.
move :: V2 Double -> Form e -> Form e
move trans form = form { formPos = formPos form + trans }

-- | Scale a form by a scalar factor. Scaling by 2 will double the size
-- of the form, and scaling by 0.5 will half the size. Note that like
-- 'move', the scale function is relative - i.e. if you scaled by 0.5
-- and then scaled by 0.5 a gain, the final scale would be 0.25 or
-- a quarter of the form's initial scale.
scale :: Double -> Form e -> Form e
scale factor form = form { formScale = factor * formScale form }

-- | Rotate a form by a given angle (in radians).
-- Like 'move' and 'scale', the rotation is relative.
rotate :: Double -> Form e -> Form e
rotate theta form = form { formTheta = formTheta form + theta }

-- | Change the alpha value of a form (i.e. its transparency).
-- By default, forms will have an alpha value of 1, in other words,
-- they are fully opaque. Alternatively, a value of 0 will mean the
-- form is completely hidden.
alpha :: Double -> Form e -> Form e
alpha x form = form { formAlpha = x }

-- | Create a form from a `Text` structure, which in turn
-- contains all of the text values and styling. This allows
-- you to render a the text graphically (and in turn it's a regular old
-- form, so it can be translated, rotated, etc.).
text :: Text -> Form e
text = defaultForm . TextForm
