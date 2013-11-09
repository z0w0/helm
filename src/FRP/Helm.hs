{-| Contains miscellaneous utility functions and the main
    functions for interfacing with the engine. -}
module FRP.Helm (
  -- * Types
  Time,
  Engine(..),
  EngineConfig(..),
  -- * Engine
  startup,
  run,
  defaultConfig,
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

{-| A data structure describing the current engine state. -}
data Engine = Engine {
  window :: SDL.Window,
  renderer :: SDL.Renderer,
  cache :: IORef (Map.Map FilePath Cairo.Surface)
}

{-| Creates a new engine that can be run later using 'run'. -}
startup :: EngineConfig -> IO Engine
startup (EngineConfig { .. }) = do
    window <- SDL.createWindow windowTitle (SDL.Position 0 0) (SDL.Size w h) flags
    renderer <- SDL.createRenderer window (SDL.Device (-1)) [SDL.Accelerated, SDL.PresentVSync]
    cache <- newIORef Map.empty

    return Engine { window = window, renderer = renderer, cache = cache }

  where
    (w, h) = windowDimensions
    flags = [SDL.WindowShown] ++ [SDL.WindowResizable | windowIsResizable] ++ [SDL.WindowFullscreen | windowIsFullscreen]

{-| Initializes and runs the game engine. The supplied signal generator is
    constantly sampled for an element to render until the user quits.

    > import FRP.Helm
    > import qualified FRP.Helm.Window as Window
    >
    > render :: (Int, Int) -> Element
    > render (w, h) = collage w h [rect (fromIntegral w) (fromIntegral h) |> filled red]
    >
    > main :: IO ()
    > main = run defaultConfig $ lift render Window.dimensions
 -}
run :: Engine -> SignalGen (Signal Element) -> IO ()
run engine gen = finally (start gen >>= run' engine) SDL.quit

{-| A utility function called by 'run' that samples the element
    or quits the entire engine if SDL events say to do so. -}
run' :: Engine -> IO Element -> IO ()
run' engine smp = do
  continue <- run''

  when continue $ do
    smp >>= render engine
    run' engine smp

{-| A utility function called by 'run\'' that polls all SDL events
    off the stack, returning true if the game should keep running,
    false otherwise. -}
run'' :: IO Bool
run'' = do
  event <- SDL.pollEvent

  case event of
    Just event ->
      case SDL.eventData event of
        SDL.Quit -> return False
        _ -> run''
    Nothing -> return True

{-| A utility function that renders a previously sampled element
    using an engine state. -}
render :: Engine -> Element -> IO ()
render engine@(Engine { .. }) element = do
  SDL.Size w h <- SDL.getWindowSize window
  texture <- SDL.createTexture renderer SDL.PixelFormatARGB8888 SDL.TextureAccessStreaming w h

  SDL.lockTexture texture Nothing $ \(pixels, pitch) ->
    Cairo.withImageSurfaceForData pixels Cairo.FormatARGB32 w h pitch $ \surface ->
      Cairo.renderWith surface (render' w h engine element)

  SDL.renderClear renderer
  SDL.renderCopy renderer texture Nothing Nothing
  SDL.renderPresent renderer

{-| A utility function called by 'render' that is called by Cairo
    when it's ready to do rendering. -}
render' :: Int -> Int -> Engine -> Element -> Cairo.Render ()
render' w h engine element = do
  Cairo.setSourceRGB 0 0 0
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.fill

  renderElement engine element

{-| A utility function that lazily grabs an image surface from the cache,
    i.e. creating it if it's not already stored in it. -}
getSurface :: Engine -> FilePath -> IO (Cairo.Surface, Int, Int)
getSurface (Engine { cache }) src = do
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

      writeIORef cache (Map.insert src surface cached)
      return (surface, w, h)

{-| A utility function for rendering a specific element. -}
renderElement :: Engine -> Element -> Cairo.Render ()
renderElement engine (CollageElement w h center forms) = do
  Cairo.save
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.clip
  forM_ center $ uncurry Cairo.translate
  mapM_ (renderForm engine) forms
  Cairo.restore

renderElement engine (ImageElement (sx, sy) sw sh src stretch) = do
  (surface, w, h) <- Cairo.liftIO $ getSurface engine (normalise src)

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
setFillStyle :: Engine -> FillStyle -> Cairo.Render ()
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
renderForm :: Engine -> Form -> Cairo.Render ()
renderForm engine Form { .. } = withTransform formScale formTheta formX formY $
  case formStyle of
    PathForm style ~ps @ ((hx, hy) : _) -> do
      Cairo.newPath
      Cairo.moveTo hx hy
      mapM_ (uncurry Cairo.lineTo) ps
      setLineStyle style

    ShapeForm style shape -> do
      Cairo.newPath

      case shape of
        PolygonShape ~ps @ ((hx, hy) : _) -> do
          Cairo.moveTo hx hy
          mapM_ (uncurry Cairo.lineTo) ps

        RectangleShape (w, h) -> Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (cx, cy) a1 a2 r (sx, sy) -> do
          Cairo.scale sx sy
          Cairo.arc cx cy r a1 a2
          Cairo.scale 1 1

      either setLineStyle (setFillStyle engine) style

    ElementForm element -> renderElement engine element
    GroupForm mayhaps forms -> do
      Cairo.save
      forM_ mayhaps Cairo.setMatrix
      mapM_ (renderForm engine) forms
      Cairo.restore
