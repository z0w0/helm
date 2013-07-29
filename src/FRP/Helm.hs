{-| Contains miscellaneous utility functions and the main
    functions for interfacing with the engine. -}
module FRP.Helm (
  -- * Types
  Time,
  EngineConfig(..),
  -- * Engine
  run,
  defaultConfig,
  -- * Utilities
  radians,
  degrees,
  turns,
  -- * Prelude
  module Color,
  module Graphics,
  module Utilities,
) where

import Control.Exception
import Control.Monad (when)
import Data.Foldable (forM_)
import Data.IORef
import Foreign.Ptr (castPtr)
import FRP.Elerea.Simple
import FRP.Helm.Color as Color
import FRP.Helm.Graphics as Graphics
import FRP.Helm.Utilities as Utilities
import FRP.Helm.Time (Time)
import System.FilePath
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

{-| Attempt to change the window dimensions (and initialize the video mode if not already).
    Will try to get a hardware accelerated window and then fallback to a software one.
    Throws an exception if the software mode can't be used as a fallback. -}
requestDimensions :: Int -> Int -> Bool -> IO SDL.Surface
requestDimensions w h resizable =	do
    mayhaps <- SDL.trySetVideoMode w h 32 $ [SDL.HWSurface, SDL.DoubleBuf] ++ flags

    case mayhaps of
      Just screen -> return screen
      Nothing -> SDL.setVideoMode w h 32 $ SDL.SWSurface : flags

  where
    flags = [SDL.Resizable | resizable]

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

{-| A data structure describing miscellaneous initial configurations of the game window and engine. -}
data EngineConfig = EngineConfig {
  windowDimensions :: (Int, Int),
  windowIsFullscreen :: Bool,
  windowIsResizable :: Bool,
  windowTitle :: String
}

{-| Creates the default configuration for the engine. You should change the fields where necessary before passing it to 'run'. -}
defaultConfig :: EngineConfig
defaultConfig = EngineConfig {
  windowDimensions = (800, 600),
  windowIsFullscreen = False,
  windowIsResizable = True,
  windowTitle = ""
}

{-| A data structure describing the current engine state.
    This may be in userland in the future, for setting
    window dimensions, title, etc. -}
data EngineState = EngineState {
  smp :: IO Element,
  {- FIXME: we need this mutable state (unfortunately) 
     because Cairo forces us to liftIO and can't return anything 
     in the render function, where the lazy image loading takes place.
     There may be a way to do this nicely, I'm just not experienced
     enough with Haskell to know how. -}
  cache :: IORef (Map.Map FilePath Cairo.Surface)
}

{-| Creates a new engine state, spawning an empty cache spawned in an IORef. -}
newEngineState :: IO Element -> IO EngineState
newEngineState smp = do
  cache <- newIORef Map.empty

  return EngineState { smp = smp, cache = cache }

{-| Initializes and runs the game engine. The supplied signal generator is
    constantly sampled for an element to render until the user quits.

    > import FRP.Helm
    > import qualified FRP.Helm.Window as Window
    >
    > render :: (Int, Int) -> Element
    > render (w, h) = collage w h [filled red $ rect (fromIntegral w) (fromIntegral h)]
    >
    > main :: IO ()
    > main = run defaultConfig $ fmap (fmap render) Window.dimensions
 -}
run :: EngineConfig -> SignalGen (Signal Element) -> IO ()
run (EngineConfig { .. }) gen = finally SDL.quit $ do
  SDL.init [SDL.InitVideo, SDL.InitJoystick]
  SDL.rawSetCaption (Just windowTitle) Nothing
  when windowIsFullscreen $ SDL.getVideoSurface >>= SDL.toggleFullscreen
  uncurry requestDimensions windowDimensions windowIsResizable
  start gen >>= newEngineState >>= run'

{-| A utility function called by 'run' that samples the element
    or quits the entire engine if SDL events say to do so. -}
run' :: EngineState -> IO ()
run' state = do
  continue <- run''

  when continue $ smp state >>= render state >> run' state

{-| A utility function called by 'run\'' that polls all SDL events
    off the stack, returning true if the game should keep running,
    false otherwise. -}
run'' :: IO Bool
run'' = do
  event <- SDL.pollEvent

  case event of
    SDL.NoEvent -> return True
    SDL.Quit -> return False
    SDL.VideoResize w h -> requestDimensions w h True >> run''
    _ -> run''

{-| A utility function that renders a previously sampled element
    using an engine state. -}
render :: EngineState -> Element -> IO ()
render state element = SDL.getVideoSurface >>= render' state element

{-| A utility function called by 'render\'' that does
    the actual heavy lifting. -}
render' :: EngineState -> Element -> SDL.Surface -> IO ()
render' state element screen = do
    pixels <- SDL.surfaceGetPixels screen

    Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatRGB24 w h (w * 4) $ \surface ->
      Cairo.renderWith surface (render'' w h state element)

    SDL.flip screen

  where
    w = SDL.surfaceGetWidth screen
    h = SDL.surfaceGetHeight screen

{-| A utility function called by 'render\'\'' that is called by Cairo
    when it's ready to do rendering. -}
render'' :: Int -> Int -> EngineState -> Element -> Cairo.Render ()
render'' w h state element = do
  Cairo.setSourceRGB 0 0 0
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.fill

  renderElement state element

{-| A utility function that lazily grabs an image surface from the cache,
    i.e. creating it if it's not already stored in it. -}
getSurface :: EngineState -> FilePath -> IO (Cairo.Surface, Int, Int)
getSurface (EngineState { cache }) src = do
  cached <- Cairo.liftIO (readIORef cache)

  case Map.lookup src cached of
    Just surface -> do
      w <- Cairo.imageSurfaceGetWidth surface
      h <- Cairo.imageSurfaceGetHeight surface

      return (surface, w, h)
    Nothing -> do
      -- TODO: Use SDL_image to support more formats. I gave up after it was painful
      -- to convert between the two surface types safely.
      -- FIXME: Does this throw an error?
      surface <- Cairo.imageSurfaceCreateFromPNG src
      w <- Cairo.imageSurfaceGetWidth surface
      h <- Cairo.imageSurfaceGetHeight surface

      writeIORef cache (Map.insert src surface cached) >> return (surface, w, h)

{-| A utility function for rendering a specific element. -}
renderElement :: EngineState -> Element -> Cairo.Render ()
renderElement state (CollageElement w h centered forms) = do
  Cairo.save
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.clip
  when centered $ Cairo.translate (fromIntegral w / 2) (fromIntegral h / 2)
  mapM_ (renderForm state) forms
  Cairo.restore

renderElement state (ImageElement (sx, sy) sw sh src stretch) = do
  (surface, w, h) <- Cairo.liftIO $ getSurface state (normalise src)

  Cairo.save
  Cairo.translate (-fromIntegral sx) (-fromIntegral sy)

  if stretch then
    Cairo.scale (fromIntegral sw / fromIntegral w) (fromIntegral sh / fromIntegral h)
  else
    Cairo.scale 1 1

  Cairo.setSourceSurface surface 0 0
  Cairo.translate (fromIntegral sx) (fromIntegral sy)
  Cairo.rectangle 0 0 (fromIntegral sw) (fromIntegral sh)
  Cairo.fill
  Cairo.restore

renderElement _ (TextElement (Text { textColor = (Color r g b a), .. })) = do
    Cairo.save

    layout <- Pango.createLayout textUTF8

    Cairo.liftIO $ Pango.layoutSetAttributes layout [Pango.AttrFamily { paStart = i, paEnd = j, paFamily = textTypeface },
                                                     Pango.AttrWeight { paStart = i, paEnd = j, paWeight = mapFontWeight textWeight },
                                                     Pango.AttrStyle { paStart = i, paEnd = j, paStyle = mapFontStyle textStyle },
                                                     Pango.AttrSize { paStart = i, paEnd = j, paSize = textHeight }]

    Pango.PangoRectangle x y w h <- fmap snd $ Cairo.liftIO $ Pango.layoutGetExtents layout

    Cairo.translate ((-w / 2) -x) ((-h / 2) - y)
    Cairo.setSourceRGBA r g b a
    Pango.showLayout layout
    Cairo.restore

  where
    i = 0
    j = length textUTF8

{-| A utility function that maps to a Pango font weight based off our variant. -}
mapFontWeight :: FontWeight -> Pango.Weight
mapFontWeight weight = case weight of
  LightWeight  -> Pango.WeightLight
  NormalWeight -> Pango.WeightNormal
  BoldWeight   -> Pango.WeightBold

{-| A utility function that maps to a Pango font style based off our variant. -}
mapFontStyle :: FontStyle -> Pango.FontStyle
mapFontStyle style = case style of
  NormalStyle  -> Pango.StyleNormal
  ObliqueStyle -> Pango.StyleOblique
  ItalicStyle  -> Pango.StyleItalic

{-| A utility function that goes into a state of transformation and then pops it when finished. -}
withTransform :: Double -> Double -> Double -> Double -> Cairo.Render () -> Cairo.Render ()
withTransform s t x y f = Cairo.save >> Cairo.scale s s >> Cairo.translate x y >> Cairo.rotate t >> f >> Cairo.restore

{-| A utility function that sets the Cairo line cap based off of our version. -}
setLineCap :: LineCap -> Cairo.Render ()
setLineCap cap = case cap of
  FlatCap   -> Cairo.setLineCap Cairo.LineCapButt
  RoundCap  -> Cairo.setLineCap Cairo.LineCapRound
  PaddedCap -> Cairo.setLineCap Cairo.LineCapSquare

{-| A utility function that sets the Cairo line style based off of our version. -}
setLineJoin :: LineJoin -> Cairo.Render ()
setLineJoin join = case join of
  SmoothJoin    -> Cairo.setLineJoin Cairo.LineJoinRound
  SharpJoin lim -> Cairo.setLineJoin Cairo.LineJoinMiter >> Cairo.setMiterLimit lim
  ClippedJoin   -> Cairo.setLineJoin Cairo.LineJoinBevel

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a line style and then strokes afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setLineStyle :: LineStyle -> Cairo.Render ()
setLineStyle (LineStyle { lineColor = Color r g b a, .. }) = do
  Cairo.setSourceRGBA r g b a
  setLineCap lineCap
  setLineJoin lineJoin
  Cairo.setLineWidth lineWidth
  Cairo.setDash lineDashing lineDashOffset
  Cairo.stroke

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a fill style and then fills afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setFillStyle :: EngineState -> FillStyle -> Cairo.Render ()
setFillStyle _ (Solid (Color r g b a)) = do
  Cairo.setSourceRGBA r g b a
  Cairo.fill

setFillStyle state (Texture src) = do
  (surface, _, _) <- Cairo.liftIO $ getSurface state (normalise src)
  Cairo.setSourceSurface surface 0 0
  Cairo.getSource >>= flip Cairo.patternSetExtend Cairo.ExtendRepeat
  Cairo.fill

setFillStyle _ (Gradient (Linear (sx, sy) (ex, ey) points)) =
  Cairo.withLinearPattern sx sy ex ey $ \pattern -> setFillStyle' pattern points

setFillStyle _ (Gradient (Radial (sx, sy) sr (ex, ey) er points)) =
  Cairo.withRadialPattern sx sy sr ex ey er $ \pattern -> setFillStyle' pattern points

{-| A utility function that adds color stops to a pattern and then fills it. -}
setFillStyle' :: Cairo.Pattern -> [(Double, Color)] -> Cairo.Render ()
setFillStyle' pattern points = do
  Cairo.setSource pattern
  mapM_ (\(o, Color r g b a) -> Cairo.patternAddColorStopRGBA pattern o r g b a) points
  Cairo.fill

{-| A utility that renders a form. -}
renderForm :: EngineState -> Form -> Cairo.Render ()
renderForm state Form { .. } = withTransform formScale formTheta formX formY $
  case formStyle of
    PathForm style ~ps @ ((hx, hy) : _) -> do
      setLineStyle style
      Cairo.moveTo hx hy
      mapM_ (uncurry Cairo.lineTo) ps

    ShapeForm style shape -> do
      case shape of
        PolygonShape ~ps @ ((hx, hy) : _) -> do
          Cairo.newPath
          Cairo.moveTo hx hy
          mapM_ (uncurry Cairo.lineTo) ps
          Cairo.closePath

        RectangleShape (w, h) -> Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (cx, cy) a1 a2 r (sx, sy) -> do
          Cairo.scale sx sy
          Cairo.arc cx cy r a1 a2
          Cairo.scale 1 1

      either setLineStyle (setFillStyle state) style

    ElementForm element -> renderElement state element
    GroupForm mayhaps forms -> do
      Cairo.save
      forM_ mayhaps Cairo.setMatrix
      mapM_ (renderForm state) forms
      Cairo.restore
