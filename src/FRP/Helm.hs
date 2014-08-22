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
  FRP.Helm.Utilities.lift
) where

import Control.Applicative
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Bits
import Data.Foldable (forM_)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import FRP.Elerea.Simple
import FRP.Helm.Color as Color
import FRP.Helm.Graphics as Graphics
import FRP.Helm.Utilities as Utilities hiding (lift)
import qualified FRP.Helm.Utilities (lift)
import FRP.Helm.Time (Time)
import System.FilePath
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

type Helm a = StateT Engine Cairo.Render a

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
  cache :: Map.Map FilePath Cairo.Surface
}

{-| Creates a new engine that can be run later using 'run'. -}
startup :: EngineConfig -> IO Engine
startup (EngineConfig { .. }) = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title 0 0 (fromIntegral w) (fromIntegral h) wflags
    renderer <- SDL.createRenderer window (-1) rflags

    return Engine { window = window, renderer = renderer, cache = Map.empty }

  where
    (w, h) = windowDimensions
    wflags = foldl (.|.) 0 $ [SDL.windowFlagShown] ++
                             [SDL.windowFlagResizable | windowIsResizable] ++
                             [SDL.windowFlagFullscreen | windowIsFullscreen]
    rflags = (.|.) (SDL.rendererFlagPresentVSync) (SDL.rendererFlagAccelerated)

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
run'' = alloca $ \eventptr -> do
  status <- SDL.pollEvent eventptr

  if status == 1 then do
    event <- peek eventptr

    case event of
      SDL.QuitEvent _ _ -> return False
      _ -> run''
  else
    return True

{-| A utility function that renders a previously sampled element
    using an engine state. -}
render :: Engine -> Element -> IO ()
render engine@(Engine { .. }) element = alloca $ \wptr      ->
                                        alloca $ \hptr      ->
                                        alloca $ \pixelsptr ->
                                        alloca $ \pitchptr  -> do
  SDL.getWindowSize window wptr hptr

  w <- fromIntegral <$> peek wptr
  h <- fromIntegral <$> peek hptr

  -- We have to use a temporary constant here because the low-level SDL2 bindings
  -- don't seem to expose pixel format constants
  texture <- SDL.createTexture renderer 372645892 SDL.textureAccessStreaming (fromIntegral w) (fromIntegral h)

  SDL.lockTexture texture nullPtr pixelsptr pitchptr

  pixels <- peek pixelsptr
  pitch <- fromIntegral <$> peek pitchptr

  Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32 w h pitch $ \surface ->
    Cairo.renderWith surface (evalStateT (render' w h element) engine)

  SDL.unlockTexture texture

  SDL.renderClear renderer
  SDL.renderCopy renderer texture nullPtr nullPtr
  SDL.destroyTexture texture
  SDL.renderPresent renderer


{-| A utility function called by 'render' that is called by Cairo
    when it's ready to do rendering. -}
render' :: Int -> Int -> Element -> Helm ()
render' w h element = do
  lift $ do Cairo.setSourceRGB 0 0 0
            Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
            Cairo.fill

  renderElement element

{-| A utility function that lazily grabs an image surface from the cache,
    i.e. creating it if it's not already stored in it. -}
getSurface :: FilePath -> Helm (Cairo.Surface, Int, Int)
getSurface src = do
  Engine _ _ cache <- get

  case Map.lookup src cache of
    Just surface -> do
      w <- Cairo.imageSurfaceGetWidth surface
      h <- Cairo.imageSurfaceGetHeight surface

      return (surface, w, h)

    Nothing -> do
      -- TODO: Use SDL_image to support more formats. I gave up after it was painful
      -- to convert between the two surface types safely.
      -- FIXME: Does this throw an error?
      surface <- liftIO $ Cairo.imageSurfaceCreateFromPNG src
      w <- liftIO $ Cairo.imageSurfaceGetWidth surface
      h <- liftIO $ Cairo.imageSurfaceGetHeight surface

      modify (\engine -> engine{cache=Map.insert src surface cache})
      return (surface, w, h)

{-| A utility function for rendering a specific element. -}
renderElement :: Element -> Helm ()
renderElement (CollageElement w h center forms) = do
  lift $ do Cairo.save
            Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
            Cairo.clip
            forM_ center $ uncurry Cairo.translate
  mapM_ renderForm forms
  lift $ Cairo.restore

renderElement (ImageElement (sx, sy) sw sh src stretch) = do
  (surface, w, h) <- getSurface (normalise src)

  lift $ do Cairo.save
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

renderElement (TextElement (Text { textColor = (Color r g b a), .. })) = do
    lift $ Cairo.save

    layout <- lift $ Pango.createLayout textUTF8

    Cairo.liftIO $ Pango.layoutSetAttributes layout [Pango.AttrFamily { paStart = i, paEnd = j, paFamily = textTypeface },
                                                     Pango.AttrWeight { paStart = i, paEnd = j, paWeight = mapFontWeight textWeight },
                                                     Pango.AttrStyle { paStart = i, paEnd = j, paStyle = mapFontStyle textStyle },
                                                     Pango.AttrSize { paStart = i, paEnd = j, paSize = textHeight }]

    Pango.PangoRectangle x y w h <- fmap snd $ Cairo.liftIO $ Pango.layoutGetExtents layout

    lift $ do Cairo.translate ((-w / 2) -x) ((-h / 2) - y)
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
withTransform :: Double -> Double -> Double -> Double -> Helm () -> Helm ()
withTransform s t x y f = do
  lift $ Cairo.save >> Cairo.scale s s >> Cairo.translate x y >> Cairo.rotate t
  f
  lift $ Cairo.restore

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
setFillStyle :: FillStyle -> Helm ()
setFillStyle (Solid (Color r g b a)) = do
  lift $ do Cairo.setSourceRGBA r g b a
            Cairo.fill

setFillStyle (Texture src) = do
  (surface, _, _) <- getSurface (normalise src)
  lift $ do Cairo.setSourceSurface surface 0 0
            Cairo.getSource >>= flip Cairo.patternSetExtend Cairo.ExtendRepeat
            Cairo.fill

setFillStyle (Gradient (Linear (sx, sy) (ex, ey) points)) =
  lift $ Cairo.withLinearPattern sx sy ex ey $ \pattern -> setFillStyle' pattern points

setFillStyle (Gradient (Radial (sx, sy) sr (ex, ey) er points)) =
  lift $ Cairo.withRadialPattern sx sy sr ex ey er $ \pattern -> setFillStyle' pattern points

{-| A utility function that adds color stops to a pattern and then fills it. -}
setFillStyle' :: Cairo.Pattern -> [(Double, Color)] -> Cairo.Render ()
setFillStyle' pattern points = do
  Cairo.setSource pattern
  mapM_ (\(o, Color r g b a) -> Cairo.patternAddColorStopRGBA pattern o r g b a) points
  Cairo.fill

{-| A utility that renders a form. -}
renderForm :: Form -> Helm ()
renderForm Form { .. } = withTransform formScale formTheta formX formY $
  case formStyle of
    PathForm style ~ps @ ((hx, hy) : _) -> do
      lift $ do Cairo.newPath
                Cairo.moveTo hx hy
                mapM_ (uncurry Cairo.lineTo) ps
                setLineStyle style

    ShapeForm style shape -> do
      lift $ Cairo.newPath

      case shape of
        PolygonShape ~ps @ ((hx, hy) : _) -> do
          lift $ do Cairo.moveTo hx hy
                    mapM_ (uncurry Cairo.lineTo) ps

        RectangleShape (w, h) -> lift $ Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (cx, cy) a1 a2 r (sx, sy) -> do
          lift $ do Cairo.scale sx sy
                    Cairo.arc cx cy r a1 a2
                    Cairo.scale 1 1

      either (lift . setLineStyle) setFillStyle style

    ElementForm element -> renderElement element
    GroupForm mayhaps forms -> do
      lift $ do Cairo.save
                forM_ mayhaps Cairo.setMatrix
      mapM_ renderForm forms
      lift $ Cairo.restore
