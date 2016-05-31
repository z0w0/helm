{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Helm.Backend.SDL.Engine where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State

import Data.Foldable (forM_)


import Foreign.Ptr

import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Backend
import FRP.Helm.Backend.SDL.Color
import FRP.Helm.Backend.SDL.Graphics
import FRP.Helm.Sample
import FRP.Helm.Signal hiding (lift)
import System.FilePath

import qualified Data.Map as Map
import qualified SDL as SDL
import SDL.Event
import SDL.Video hiding (windowTitle)
import qualified SDL.Video as Video
import Linear.V2 (V2(V2))
import qualified SDL.Init
import qualified SDL.Video.Renderer as Renderer
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango
import Data.Text (pack)

{-| A data structure describing the current engine state. -}
data Engine = Engine {
  window   :: Window,
  renderer :: Renderer,
  cache    :: Map.Map FilePath Cairo.Surface,
  continue :: Bool
}

--data Application = Application {
--  mainElement    :: Element,
--  mainDimensions :: (Int, Int),
--  mainContinue   :: Bool
--}

data EngineConfig = EngineConfig {
  windowDimensions :: (Int, Int),
  windowIsFullscreen :: Bool,
  windowIsResizable :: Bool,
  windowTitle :: String
}

instance BEngine Engine where
  type RenderMonad Engine = Cairo.Render

  {-| A data structure holding the main element and information required for
      rendering. -}
  data BApplication Engine = Application {
    mainElement    :: Element,
    mainDimensions :: (Int, Int),
    mainContinue   :: Bool
  }

  {-| A data structure describing miscellaneous initial configurations of the
      game window and engine. -}
  type BConfig Engine = EngineConfig


  type BElement Engine = Element

  {-| Creates the default configuration for the engine. You should change the
  fields where necessary before passing it to 'run'. -}
  defaultConfig = EngineConfig {
    windowDimensions = (800, 600),
    windowIsFullscreen = False,
    windowIsResizable = True,
    windowTitle = ""
  }

  initApplication e d c _ = Application e d c
  engineFinalizer = const SDL.Init.quit
  continueExecution = continue

  {-| Creates a new engine that can be run later using 'run'. -}
  startup :: EngineConfig -> IO Engine
  startup (EngineConfig { .. }) = do
      SDL.Init.initializeAll
      window <- createWindow (pack windowTitle) winCfg
      renderer <- createRenderer window (-1) renCfg
      showWindow window

      return Engine { window   = window
                    , renderer = renderer
                    , cache    = Map.empty
                    , continue = True
                    }

    where
      (w, h) = windowDimensions
      winCfg = defaultWindow { windowInitialSize = V2 (fromIntegral w) (fromIntegral h)
                             , windowMode = if windowIsFullscreen then Fullscreen else Windowed
                             , windowResizable = windowIsResizable
                             }
      renCfg = RendererConfig AcceleratedVSyncRenderer False

  {-| An event that triggers when SDL thinks we need to re-draw. -}
  --exposedSignal :: BEngine engine => Signal engine ()
  exposedSignal = Signal getExposed
    where
      getExposed = effectful $ do
        pumpEvents
        mEvent <- pollEvent
        case mEvent of
          Just (Event _ (WindowExposedEvent _)) ->
            return $ Changed ()
          _ ->
            return $ Unchanged ()

  {-| An event that triggers when SDL thinks we need to quit. -}
  --quit :: Signal ()
  quitSignal = Signal getQuit
    where
      getQuit = effectful $ do
        mEvent <- pollEvent
        case mEvent of
          Just (Event _ QuitEvent) ->
            return $ Changed ()
          _ ->
            return $ Unchanged ()
            {-| A utility function that renders a previously sampled element
                using an engine state. -}

-- TODO: Refactor

  {-| Renders when the sample is marked as changed delays the thread otherwise -}
  renderIfChanged :: Engine -> Sample (BApplication Engine) -> IO Engine
  renderIfChanged engine event =  case event of
      Changed   app -> if mainContinue app
                       then render engine (mainElement app) (mainDimensions app)
                       else return engine { continue = False }

      Unchanged _ -> do threadDelay 1000
                        return engine

  {-| The current dimensions of the window. -}
  dimensions :: Signal Engine (Int, Int)
  dimensions =
    Signal $ input >>= getDimensions >>= transfer (pure (0,0)) update
    where
      getDimensions = effectful1 action
      action engine = do
        V2 w h <- SDL.get $ Video.windowSize (window engine)
        return (fromIntegral w, fromIntegral h)

  {-| The current position of the window. -}
  position :: Signal Engine (Int, Int)
  position =
    Signal $ input >>= getPosition >>= transfer (pure (0,0)) update
    where
      getPosition = effectful1 action
      action engine = do
          V2 x y <- Video.getWindowAbsolutePosition (window engine)
          return (fromIntegral x, fromIntegral y)

{-| A utility function that renders a previously sampled element
    using an engine state. -}
render :: Engine -> Element -> (Int, Int) -> IO Engine
render engine@(Engine { .. }) element (w, h) = do
  texture <- Renderer.createTexture renderer Renderer.ARGB8888
               Renderer.TextureAccessStreaming (V2 (fromIntegral w) (fromIntegral h))

  (pixels, pitch) <- Renderer.lockTexture texture Nothing

  res <- Cairo.withImageSurfaceForData (castPtr pixels)
           Cairo.FormatARGB32 w h (fromIntegral pitch) $ \surface -> Cairo.renderWith surface
             $ evalStateT (render' w h element) engine

  Renderer.unlockTexture texture

  Renderer.clear renderer
  Renderer.copy renderer texture Nothing Nothing
  Renderer.destroyTexture texture
  Renderer.present renderer

  return res


{-| A utility function called by 'render' that is called by Cairo
    when it's ready to do rendering. -}
render' :: Int -> Int -> Element -> Helm Engine Engine
render' w h element = do
  lift $ do Cairo.setSourceRGB 0 0 0
            Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
            Cairo.fill

  renderElement element
  get

{-| A utility function that lazily grabs an image surface from the cache,
    i.e. creating it if it's not already stored in it. -}
getSurface :: FilePath -> Helm Engine (Cairo.Surface, Int, Int)
getSurface src = do
  Engine _ _ cache _ <- get

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
renderElement :: Element -> Helm Engine ()
renderElement (CollageElement w h center forms) = do
  lift $ do Cairo.save
            Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
            Cairo.clip
            forM_ center $ uncurry Cairo.translate
  mapM_ renderForm forms
  lift Cairo.restore

renderElement (ImageElement (sx, sy) sw sh src stretch) = do
  (surface, w, h) <- getSurface (normalise src)

  lift $ do Cairo.save
            Cairo.translate (-fromIntegral sx) (-fromIntegral sy)

            if stretch then
              Cairo.scale (fromIntegral sw / fromIntegral w)
                (fromIntegral sh / fromIntegral h)
            else
              Cairo.scale 1 1

            Cairo.setSourceSurface surface 0 0
            Cairo.translate (fromIntegral sx) (fromIntegral sy)
            Cairo.rectangle 0 0 (fromIntegral sw) (fromIntegral sh)
            if stretch then
                Cairo.paint
            else
                Cairo.fill

            Cairo.restore

renderElement (TextElement (Text { textColor = (Color r g b a), .. })) = do
    lift Cairo.save

    layout <- lift $ Pango.createLayout textUTF8

    Cairo.liftIO $ Pango.layoutSetAttributes layout
      [ Pango.AttrFamily { paStart = i, paEnd = j, paFamily = pack textTypeface }
      , Pango.AttrWeight { paStart = i, paEnd = j, paWeight = mapFontWeight textWeight }
      , Pango.AttrStyle  { paStart = i, paEnd = j, paStyle = mapFontStyle textStyle }
      , Pango.AttrSize   { paStart = i, paEnd = j, paSize = textHeight }
      ]

    Pango.PangoRectangle x y w h <- fmap snd
      $ Cairo.liftIO $ Pango.layoutGetExtents layout

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

{-| A utility function that goes into a state of transformation and then pops
    it when finished. -}
withTransform :: Double -> Double -> Double -> Double -> Helm Engine () -> Helm Engine ()
withTransform s t x y f = do
  lift $ Cairo.save >> Cairo.scale s s >> Cairo.translate x y >> Cairo.rotate t
  f
  lift Cairo.restore

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
setFillStyle :: FillStyle -> Helm Engine ()
setFillStyle (Solid (Color r g b a)) = lift $ do
  Cairo.setSourceRGBA r g b a
  Cairo.fill

setFillStyle (Texture src) = do
  (surface, _, _) <- getSurface (normalise src)
  lift $ do Cairo.setSourceSurface surface 0 0
            Cairo.getSource >>= flip Cairo.patternSetExtend Cairo.ExtendRepeat
            Cairo.fill

setFillStyle (Gradient (Linear (sx, sy) (ex, ey) points)) =
  lift $ Cairo.withLinearPattern sx sy ex ey
       $ \pattern -> setFillStyle' pattern points

setFillStyle (Gradient (Radial (sx, sy) sr (ex, ey) er points)) =
  lift $ Cairo.withRadialPattern sx sy sr ex ey er
       $ \pattern -> setFillStyle' pattern points

{-| A utility function that adds color stops to a pattern and then fills it. -}
setFillStyle' :: Cairo.Pattern -> [(Double, Color)] -> Cairo.Render ()
setFillStyle' pattern points = do
  Cairo.setSource pattern
  mapM_ (\(o, Color r g b a) -> Cairo.patternAddColorStopRGBA pattern o r g b a) points
  Cairo.fill

{-| A utility that renders a form. -}
renderForm :: Form -> Helm Engine ()
renderForm Form { .. } = withTransform formScale formTheta formX formY $
  case formStyle of
    PathForm style ~ps @ ((hx, hy) : _) -> lift $ do
      Cairo.newPath
      Cairo.moveTo hx hy
      mapM_ (uncurry Cairo.lineTo) ps
      setLineStyle style

    ShapeForm style shape -> do
      lift Cairo.newPath

      case shape of
        PolygonShape ~ps @ ((hx, hy) : _) ->
          lift $ do Cairo.moveTo hx hy
                    mapM_ (uncurry Cairo.lineTo) ps

        RectangleShape (w, h) -> lift $ Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (cx, cy) a1 a2 r (sx, sy) ->
          lift $ do Cairo.scale sx sy
                    Cairo.arc cx cy r a1 a2
                    Cairo.scale 1 1

      either (lift . setLineStyle) setFillStyle style

    ElementForm element -> renderElement element
    GroupForm mayhaps forms -> do
      lift $ do Cairo.save
                forM_ mayhaps Cairo.setMatrix
      mapM_ renderForm forms
      lift Cairo.restore
