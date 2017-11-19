-- | Contains the SDL engine types.
module Helm.Engine.SDL.Engine
  (
    -- * Types
    SDLEngine(..)
  , SDLEngineConfig(..)
    -- * Startup
  , defaultConfig
  , startup
  , startupWith
    -- * Asset Loading
  , withImage
  ) where

import           Control.Monad (when)

import           Data.Foldable (forM_)
import qualified Data.Text as T
import           Data.Word (Word32)
import           Foreign.Ptr (castPtr)

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.Rendering.Cairo.Matrix (Matrix(..))
import qualified Graphics.Rendering.Pango as Pango
import           FRP.Elerea.Param (Signal, SignalGen, externalMulti)
import           Linear.Affine (Point(P))
import           Linear.Metric (distance)
import           Linear.V2 (V2(V2))
import           Linear.V3 (V3(V3))

import qualified SDL
import qualified SDL.Event as Event
import qualified SDL.Init as Init
import           SDL.Input.Keyboard (Keysym(..))
import qualified SDL.Time as Time
import qualified SDL.Video as Video
import           SDL.Video (WindowConfig(..))
import qualified SDL.Video.Renderer as Renderer

import           Helm.Asset (Image)
import           Helm.Color (Color(..), Gradient(..))
import           Helm.Engine (Engine(..), Key, MouseButton)
import           Helm.Engine.SDL.Keyboard (mapKey)
import           Helm.Engine.SDL.Mouse (mapMouseButton)
import           Helm.Graphics
import qualified Helm.Graphics2D as Graphics2D
import           Helm.Graphics2D.Text (Text(..), FontWeight(..), FontStyle(..),
                                       TextAlignment(..), Alignment(..))

-- | Represents the configuration to run the SDL engine with.
-- Use 'defaultConfig' and then only change the necessary fields.
data SDLEngineConfig = SDLEngineConfig
  { windowDimensions :: V2 Int   -- ^ The initial SDL window size.
  , windowIsFullscreen :: !Bool  -- ^ Whether the SDL window should start fullscreen.
  , windowIsResizable :: !Bool   -- ^ Whether the SDL window should be resizable.
  , windowTitle :: !String       -- ^ The initial SDL window title.
  }

-- | Represents the SDL engine's internal state.
data SDLEngine = SDLEngine
  { window :: !Video.Window                                                        -- ^ The SDL window.
  , renderer :: !Video.Renderer                                                    -- ^ The SDL renderer.
  , texture :: !Renderer.Texture                                                   -- ^ The SDL texture for rendering.
  , engineConfig :: !SDLEngineConfig                                               -- ^ The engine config being used.
  , lastMousePress :: Maybe (Word32, V2 Double)                                    -- ^ The last mouse press info.

  , mouseMoveEventSignal :: SignalGen SDLEngine (Signal [V2 Int])                  -- ^ The mouse move event signal.
  , mouseMoveEventSink :: V2 Int -> IO ()                                          -- ^ The mouse move event sink.
  , mouseDownEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])   -- ^ The mouse down event signal.
  , mouseDownEventSink :: (MouseButton, V2 Int) -> IO ()                           -- ^ The mouse down event sink.
  , mouseUpEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])     -- ^ The mouse up event signal.
  , mouseUpEventSink :: (MouseButton, V2 Int) -> IO ()                             -- ^ The mouse up event sink.
  , mouseClickEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])  -- ^ The mouse click event signal.
  , mouseClickEventSink :: (MouseButton, V2 Int) -> IO ()                          -- ^ The mouse click event sink.

  , keyboardDownEventSignal :: SignalGen SDLEngine (Signal [Key])                  -- ^ The keyboard down event signal.
  , keyboardDownEventSink :: Key -> IO ()                                          -- ^ The keyboard down event sink.
  , keyboardUpEventSignal :: SignalGen SDLEngine (Signal [Key])                    -- ^ The keyboard up event signal.
  , keyboardUpEventSink :: Key -> IO ()                                            -- ^ The keyboard up event sink.
  , keyboardPressEventSignal :: SignalGen SDLEngine (Signal [Key])                 -- ^ The keyboard press event signal.
  , keyboardPressEventSink :: Key -> IO ()                                         -- ^ The keyboard press event sink.

  , windowResizeEventSignal :: SignalGen SDLEngine (Signal [V2 Int])               -- ^ The window resize event signal.
  , windowResizeEventSink :: V2 Int -> IO ()                                       -- ^ The window resize event sink.
  }

-- | Represents an 'Image' for the SDL engine.
data instance Image SDLEngine = SDLImage
  { cairoSurface :: Cairo.Surface -- ^ The Cairo surface for the image.
  , imageDims    :: V2 Int        -- ^ The image dimensions of the image (when it was loaded).
  }

-- | Load an  image asset using the SDL engine and do
-- something with it. The image will be cleaned up
-- once the provided monad completes.
--
-- Currently, the only supported image file format is PNG.
--
-- The expected usage would be to use 'withImage'
-- for each image you need to load before
-- running the engine, and then use the images with
-- graphics. Once the engine stops running, the image
-- will then be automatically cleaned up.
withImage :: SDLEngine -> FilePath -> (Image SDLEngine -> IO a) -> IO a
withImage _ path f = Cairo.withImageSurfaceFromPNG path $ \surface -> do
  width  <- Cairo.imageSurfaceGetWidth surface
  height <- Cairo.imageSurfaceGetHeight surface

  f SDLImage
    { cairoSurface = surface
    , imageDims    = V2 width height
    }

-- | Provides the SDL engine implementation for Helm.
instance Engine SDLEngine where
  -- | Render the SDL engine.
  render engine (Graphics2D coll) = render2d engine coll

  -- | Cleanup the engine assets and quit using SDL's init library.
  cleanup SDLEngine { window, renderer, texture } = do
    Renderer.destroyTexture texture
    Video.destroyWindow window
    Video.destroyRenderer renderer
    Init.quit

  -- | Tick the engine forward.
  --
  -- At the moment this just sinks event into the signals
  -- as no other functionality is required.
  tick engine = do
    mayhaps <- Event.pumpEvents >> Event.pollEvent

    case mayhaps of
      -- Handle the quit event exclusively first to simplify our code
      Just Event.Event { eventPayload = Event.QuitEvent } ->
        return Nothing

      -- Sink everything else into the signals
      Just Event.Event { .. } ->
        sinkEvent engine eventPayload >>= tick

      Nothing -> return $ Just engine

  -- | The SDL-specific mouse move signal.
  mouseMoveSignal = mouseMoveEventSignal

  -- | The SDL-specific mouse down signal.
  mouseDownSignal = mouseDownEventSignal

  -- | The SDL-specific mouse up signal.
  mouseUpSignal = mouseUpEventSignal

  -- | The SDL-specific mouse click signal.
  mouseClickSignal = mouseClickEventSignal

  -- | The SDL-specific keyboard down signal.
  keyboardDownSignal = keyboardDownEventSignal

  -- | The SDL-specific keyboard up signal.
  keyboardUpSignal = keyboardUpEventSignal

  -- | The SDL-specific keyboard press signal.
  keyboardPressSignal = keyboardPressEventSignal

  -- | The SDL-specific window resize signal.
  windowResizeSignal = windowResizeEventSignal

  -- | The running time provided using SDL ticks. This may
  -- need to be changed to a Haskell library for consistency with other future engines.
  runningTime _ = fromIntegral <$> Time.ticks

  -- | Gets the size of the SDL engine's window.
  windowSize SDLEngine { window } = fmap (fmap fromIntegral) . SDL.get $ Video.windowSize window

-- | The default configuration for the engine. You should change the values where necessary.
defaultConfig :: SDLEngineConfig
defaultConfig = SDLEngineConfig
  { windowDimensions = V2 800 600
  , windowIsFullscreen = False
  , windowIsResizable = True
  , windowTitle = "Helm"
  }

-- | Initialize a new engine with default configuration. The engine can then be run later using 'run'.
startup :: IO SDLEngine
startup = startupWith defaultConfig

-- | Prepare a texture for streamed rendering based of a window size.
prepTexture
  :: V2 Int               -- ^ The dimensions for the new texture.
  -> Video.Renderer       -- ^ The SDL video renderer.
  -> IO Renderer.Texture  -- ^ An IO monad that produces the prepared SDL texture.
prepTexture dims renderer =
  Renderer.createTexture renderer mode access $ fromIntegral <$> dims

  where
    mode = Renderer.ARGB8888
    access = Renderer.TextureAccessStreaming

-- | Initialize a new engine with some configration, ready to be 'run'.
startupWith
  :: SDLEngineConfig  -- ^ The configuration to start the engine with.
  -> IO SDLEngine     -- ^ An IO monad that produces an SDL engine to run games with.
startupWith config@SDLEngineConfig { .. } = do
  Init.initializeAll

  -- Initialize the SDL window from our provided engine config.
  window <- Video.createWindow (T.pack windowTitle) windowConfig
  renderer <- Video.createRenderer window (-1) rendererConfig
  texture <- prepTexture windowDimensions renderer

  -- Initialize all of the sinks and signals that SDL events will be sunk into.
  mouseMoveEvent <- externalMulti
  mouseDownEvent <- externalMulti
  mouseUpEvent <- externalMulti
  mouseClickEvent <- externalMulti
  keyboardDownEvent <- externalMulti
  keyboardUpEvent <- externalMulti
  keyboardPressEvent <- externalMulti
  windowResizeEvent <- externalMulti

  -- By default the SDL window isn't shown
  Video.showWindow window

  return SDLEngine
    { window = window
    , renderer = renderer
    , texture = texture
    , engineConfig = config
    , lastMousePress = Nothing

    , mouseMoveEventSignal = fst mouseMoveEvent
    , mouseMoveEventSink = snd mouseMoveEvent
    , mouseDownEventSignal = fst mouseDownEvent
    , mouseDownEventSink = snd mouseDownEvent
    , mouseUpEventSignal = fst mouseUpEvent
    , mouseUpEventSink = snd mouseUpEvent
    , mouseClickEventSignal = fst mouseClickEvent
    , mouseClickEventSink = snd mouseClickEvent

    , keyboardDownEventSignal = fst keyboardDownEvent
    , keyboardDownEventSink = snd keyboardDownEvent
    , keyboardUpEventSignal = fst keyboardUpEvent
    , keyboardUpEventSink = snd keyboardUpEvent
    , keyboardPressEventSignal = fst keyboardPressEvent
    , keyboardPressEventSink = snd keyboardPressEvent

    , windowResizeEventSignal = fst windowResizeEvent
    , windowResizeEventSink = snd windowResizeEvent
    }

  where
    rendererConfig = Video.RendererConfig Video.AcceleratedVSyncRenderer False
    windowConfig = Video.defaultWindow
      { windowInitialSize = fromIntegral <$> windowDimensions
      , windowMode = if windowIsFullscreen
                     then Video.Fullscreen
                     else Video.Windowed
      , windowResizable = windowIsResizable
      }

-- | Renders a 2D element to the engine screen.
render2d
  :: SDLEngine                     -- ^ The SDL engine to use for rendering.
  -> Graphics2D.Collage SDLEngine  -- ^ The collage to render.
  -> IO ()                         -- ^ An IO monad that renders the SDL collage.
render2d SDLEngine { window, renderer, texture } coll = do
  V2 w h <- SDL.get $ Video.windowSize window

  (pixels, pitch) <- Renderer.lockTexture texture Nothing

  Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h) (fromIntegral pitch) $ \surface ->
    Cairo.renderWith surface $ do
      Cairo.setSourceRGB 0 0 0
      Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
      Cairo.fill

      renderCollage coll

  Renderer.unlockTexture texture
  Renderer.clear renderer
  Renderer.copy renderer texture Nothing Nothing
  Renderer.present renderer

-- | Render a collage (a group of forms with context).
renderCollage
  :: Graphics2D.Collage SDLEngine  -- ^ The collage to render.
  -> Cairo.Render ()               -- ^ The render monad result.
renderCollage Graphics2D.Collage { .. } = do
  Cairo.save

  forM_ collageDims $ \(V2 w h) -> do
    Cairo.rectangle 0 0 w h
    Cairo.clip

  forM_ collageCenter $ \(V2 x y) -> Cairo.translate x y
  mapM_ renderForm collageForms

  Cairo.restore

-- | Map a 'FontWeight' to a Pango font weight.
mapFontWeight :: FontWeight -> Pango.Weight
mapFontWeight weight = case weight of
  LightWeight  -> Pango.WeightLight
  NormalWeight -> Pango.WeightNormal
  BoldWeight   -> Pango.WeightBold

-- | Map a 'FontStyle' variant to a Pango font style.
mapFontStyle :: FontStyle -> Pango.FontStyle
mapFontStyle style = case style of
  NormalStyle  -> Pango.StyleNormal
  ObliqueStyle -> Pango.StyleOblique
  ItalicStyle  -> Pango.StyleItalic

-- | Setup a transformation state, render something with it, and then restore the old state.
withTransform
  :: Double           -- ^ The x and y scale factor of the state.
  -> Double           -- ^ The theta rotation of the state, in radians.
  -> Double           -- ^ The x translation value for the state.
  -> Double           -- ^ The y translation value for the state.
  -> Cairo.Render ()  -- ^ The render monad to run with the transformation state.
  -> Cairo.Render ()  -- ^ The final render monad.
withTransform s t x y f = do
  Cairo.save
  Cairo.scale s s
  Cairo.translate x y
  Cairo.rotate t
  f
  Cairo.restore

-- | Set the Cairo line cap from a 'LineCap'.
setLineCap
  :: Graphics2D.LineCap  -- ^ The line cap to use for rendering.
  -> Cairo.Render ()     -- ^ The render monad with the line cap set.
setLineCap cap = case cap of
  Graphics2D.FlatCap   -> Cairo.setLineCap Cairo.LineCapButt
  Graphics2D.RoundCap  -> Cairo.setLineCap Cairo.LineCapRound
  Graphics2D.PaddedCap -> Cairo.setLineCap Cairo.LineCapSquare

-- | Set the Cairo line join from a 'LineJoin'.
setLineJoin
  :: Graphics2D.LineJoin  -- ^ The line join to use for rendering.
  -> Cairo.Render ()      -- ^ The render monad with the line join set.
setLineJoin join = case join of
  Graphics2D.SmoothJoin    -> Cairo.setLineJoin Cairo.LineJoinRound
  Graphics2D.ClippedJoin   -> Cairo.setLineJoin Cairo.LineJoinBevel
  Graphics2D.SharpJoin lim -> do
    Cairo.setLineJoin Cairo.LineJoinMiter
    Cairo.setMiterLimit lim

-- | Set up all the necessary settings with Cairo
-- to render with a line style (and then stroke the line). Assumes
-- that all drawing paths have already been setup before being called.
setLineStyle
  :: Graphics2D.LineStyle  -- ^ The line style to use for rendering.
  -> Cairo.Render ()       -- ^ The render monad with the line style set.
setLineStyle Graphics2D.LineStyle { lineColor = Color r g b a, .. } = do
  Cairo.setSourceRGBA r g b a
  setLineCap lineCap
  setLineJoin lineJoin
  Cairo.setLineWidth lineWidth
  Cairo.setDash lineDashing lineDashOffset
  Cairo.stroke

-- | Set up all the necessary settings with Cairo
-- to render with a fill style (and then fill the line). Assumes
-- that all drawing paths have already been setup before being called.
setFillStyle
  :: Graphics2D.FillStyle SDLEngine  -- ^ The fill style to use for rendering.
  -> Cairo.Render ()                 -- ^ The render monad with the fill style set.
setFillStyle (Graphics2D.Solid (Color r g b a)) = do
  Cairo.setSourceRGBA r g b a
  Cairo.fill

-- Fill with a texture.
setFillStyle (Graphics2D.Texture SDLImage { cairoSurface }) = do
  Cairo.setSourceSurface cairoSurface 0 0
  Cairo.getSource >>= flip Cairo.patternSetExtend Cairo.ExtendRepeat
  Cairo.fill

--  Fill with a linear gradient.
setFillStyle (Graphics2D.Gradient (Linear (sx, sy) (ex, ey) points)) =
  Cairo.withLinearPattern sx sy ex ey $ \ptn ->
    setGradientFill ptn points

-- Fill with a radial gradient.
setFillStyle (Graphics2D.Gradient (Radial (sx, sy) sr (ex, ey) er points)) =
  Cairo.withRadialPattern sx sy sr ex ey er $ \ptn ->
    setGradientFill ptn points

-- | Add color stops to a pattern and then fill it.
setGradientFill
  :: Cairo.Pattern      -- ^ The pattern to set for filling.
  -> [(Double, Color)]  -- ^ The gradient points to use for filling.
  -> Cairo.Render ()    -- ^ The render monad result.
setGradientFill ptn points = do
  Cairo.setSource ptn
  mapM_ (\(o, Color r g b a) -> Cairo.patternAddColorStopRGBA ptn o r g b a) points
  Cairo.fill

-- | Render a form.
renderForm
  :: Graphics2D.Form SDLEngine  -- ^ The form to render.
  -> Cairo.Render ()            -- ^ The render monad result.
renderForm Graphics2D.Form { formPos = V2 x y, .. } = withTransform formScale formTheta x y $ do
  Cairo.save

  case formStyle of
    -- Render a path form (a connection of points).
    Graphics2D.PathForm style (Graphics2D.Path (~ps @ (V2 hx hy : _))) -> do
      Cairo.newPath
      Cairo.moveTo hx hy
      mapM_ (\(V2 lx ly) -> Cairo.lineTo lx ly) ps
      setLineStyle style

    -- Render a shape (a multitude generalised path form).
    Graphics2D.ShapeForm style shape -> do
      Cairo.newPath

      case shape of
        Graphics2D.PolygonShape (Graphics2D.Path (~ps @ (V2 hx hy : _))) -> do
          Cairo.moveTo hx hy
          mapM_ (\(V2 lx ly) -> Cairo.lineTo lx ly) (ps ++ [head ps])

        Graphics2D.RectangleShape (V2 w h) ->
          Cairo.rectangle (-w / 2) (-h / 2) w h

        Graphics2D.ArcShape (V2 cx cy) a1 a2 r (V2 sx sy) -> do
          Cairo.scale sx sy
          Cairo.arc cx cy r a1 a2

      case style of
        Graphics2D.OutlinedShape ls -> setLineStyle ls
        Graphics2D.FilledShape   fs -> setFillStyle fs

    -- Render a text form using Pango.
    Graphics2D.TextForm Text { textColor = Color r g b a, .. } -> do
      layout <- Pango.createLayout textString

      Cairo.liftIO $ Pango.layoutSetAttributes layout
        [ Pango.AttrFamily { paStart = i, paEnd = j, paFamily = T.pack textTypeface }
        , Pango.AttrWeight { paStart = i, paEnd = j, paWeight = mapFontWeight textWeight }
        , Pango.AttrStyle  { paStart = i, paEnd = j, paStyle = mapFontStyle textStyle }
        , Pango.AttrSize   { paStart = i, paEnd = j, paSize = textHeight }
        ]

      Pango.PangoRectangle tx ty w h <- fmap snd $ Cairo.liftIO $ Pango.layoutGetExtents layout

      let alignOffset extent AlignBefore = -extent
          alignOffset extent AlignCenter = -extent/2
          alignOffset extent AlignAfter  = 0
      Cairo.translate
        (alignOffset w (horizontalAlignment textAlign) - tx)
        (alignOffset h (verticalAlignment textAlign)   - ty)
      Cairo.setSourceRGBA r g b a
      Pango.showLayout layout

      where
        i = 0
        j = length textString

    -- Render an image form.
    Graphics2D.ImageForm SDLImage { imageDims = V2 w h, .. } (V2 sx sy) (V2 sw sh) stretch -> do
      Cairo.translate (-sx) (-sy)

      if stretch then
        Cairo.scale (sw / fromIntegral w)
                    (sh / fromIntegral h)
      else
        Cairo.scale 1 1

      Cairo.setSourceSurface cairoSurface 0 0
      Cairo.translate sx sy
      Cairo.rectangle 0 0 sw sh

      if stretch then
        Cairo.paint
      else
        Cairo.fill

    -- Render a group of other forms using a transform.
    Graphics2D.GroupForm (Graphics2D.Transform (V3 (V3 a b lx) (V3 c d ly) _)) forms -> do
      Cairo.transform $ Matrix a b c d lx ly
      mapM_ renderForm forms

    -- Render a collage within a collage.
    Graphics2D.CollageForm coll -> renderCollage coll

  Cairo.restore

-- | Turns a point containing a vector into a regular vector.
depoint :: Point f a -> f a
depoint (P x) = x

-- | Sink an SDL event into the Elerea sinks initialized at startup of the SDL engine.
--
-- These sinks then provide the data for the Elerea signals, which will be in
-- turn will provide the Helm subscriptions with events.
sinkEvent
  :: SDLEngine           -- ^ The SDL engine to sink events into.
  -> Event.EventPayload  -- ^ The SDL event payload to sink.
  -> IO SDLEngine        -- ^ An IO-monad that produces the SDL engine with its events sunk.
sinkEvent engine (Event.WindowResizedEvent Event.WindowResizedEventData { .. }) = do
  windowResizeEventSink engine dims
  Renderer.destroyTexture texture

  -- Create a new texture with the correct size matching the window dims.
  resized <- prepTexture dims renderer

  return engine { texture = resized }

  where
    dims = fromIntegral <$> windowResizedEventSize
    SDLEngine { texture, renderer } = engine

-- Sink mouse motion events as mouse moves.
sinkEvent engine (Event.MouseMotionEvent Event.MouseMotionEventData { .. }) = do
  mouseMoveEventSink engine $ fromIntegral <$> depoint mouseMotionEventPos

  return engine

-- Sink keyboard events into the relevant Elerea sinks.
--
-- Note that keyboard up and press are the same for the time being.
-- This may change in the future.
sinkEvent engine (Event.KeyboardEvent Event.KeyboardEventData { .. }) =
  case keyboardEventKeyMotion of
    Event.Pressed -> do
      keyboardDownEventSink engine key

      if keyboardEventRepeat
      then keyboardPressEventSink engine key >> return engine
      else return engine

    Event.Released -> do
      keyboardUpEventSink engine key
      keyboardPressEventSink engine key

      return engine

  where
    Keysym { .. } = keyboardEventKeysym
    key = mapKey keysymKeycode

-- Sink mouse events into the relevant Elerea sinks.
sinkEvent engine (Event.MouseButtonEvent Event.MouseButtonEventData { .. }) =
  case mouseButtonEventMotion of
    Event.Pressed -> do
      ticks <- Time.ticks
      mouseDownEventSink engine tup

      return engine { lastMousePress = Just (ticks, dubPos) }

    Event.Released -> do
      mouseUpEventSink engine tup

      -- Weirdly enough, SDL provides a value that says how many clicks there
      -- were, but this value is always set to one even if it's just a regular
      -- mouse up event. Note that here we're defining a click as a mouse up
      -- event being in a very close proximity to a previous mouse down event.
      -- We manually calculate whether this was a click or not.
      case lastMousePress of
        Just (lastTicks, lastPos) -> do
          ticks <- Time.ticks

          -- Check that it's a expected amount of time for a click and that the mouse
          -- has basically stayed in place
          when (distance dubPos lastPos <= clickRadius && ticks - lastTicks < clickMs)
               (mouseClickEventSink engine tup)

        Nothing -> return ()

      return engine

  where
    SDLEngine { lastMousePress } = engine
    clickMs = 500    -- How long between mouse down/up to recognise clicks
    clickRadius = 3  -- The pixel radius to be considered a click.
    pos = depoint mouseButtonEventPos
    dubPos = fromIntegral <$> pos
    tup = (mapMouseButton mouseButtonEventButton, fromIntegral <$> pos)

-- Don't sink other events.
sinkEvent engine _ = return engine
