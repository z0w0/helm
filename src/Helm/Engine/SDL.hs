{-| Contains the SDL implementation of Helm. -}
module Helm.Engine.SDL
  (
   -- * Types
   SDLEngine
  ,SDLEngineConfig(..)
  ,
   -- * Utilities
   defaultConfig
  ,startup
  ,startupWith)
  where

import           Data.Int (Int32)
import qualified Data.Text as T
import           Data.Word (Word32)

import           FRP.Elerea.Param
import           Linear.Affine (Point(P))
import           Linear.V2 (V2(V2))
import qualified SDL
import qualified SDL.Event as Event
import qualified SDL.Init as Init
import           SDL.Input.Keyboard (Keysym(..))
import qualified SDL.Time as Time
import qualified SDL.Video as Video
import           SDL.Video (WindowConfig(..))
import qualified SDL.Video.Renderer as Renderer

import           Helm.Asset
import           Helm.Engine (Engine(..), Key, MouseButton)
import           Helm.Graphics (Graphics(..))
import           Helm.Graphics2D (Element)
import           Helm.Engine.SDL.Keyboard (mapKey)
import           Helm.Engine.SDL.Mouse (mapMouseButton)
import qualified Helm.Engine.SDL.Graphics2D as Graphics2D

-- | A data structure describing how to run the SDL engine.
-- Use 'defaultConfig' and then only change the data fields that you need to.
data SDLEngineConfig = SDLEngineConfig
  { windowDimensions :: (Int, Int)
  , windowIsFullscreen :: Bool
  , windowIsResizable :: Bool
  , windowTitle :: String
  }

-- | A data structure describing the SDL engine's state.
data SDLEngine = SDLEngine
  { window :: Video.Window
  , renderer :: Video.Renderer
  , engineConfig :: SDLEngineConfig
  , lastMousePress :: Maybe (Word32, V2 Int32)

  , mouseMoveEventSignal :: SignalGen SDLEngine (Signal [V2 Int])
  , mouseMoveEventSink :: V2 Int -> IO ()
  , mouseDownEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])
  , mouseDownEventSink :: (MouseButton, V2 Int) -> IO ()
  , mouseUpEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])
  , mouseUpEventSink :: (MouseButton, V2 Int) -> IO ()
  , mouseClickEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])
  , mouseClickEventSink :: (MouseButton, V2 Int) -> IO ()

  , keyboardDownEventSignal :: SignalGen SDLEngine (Signal [Key])
  , keyboardDownEventSink :: Key -> IO ()
  , keyboardUpEventSignal :: SignalGen SDLEngine (Signal [Key])
  , keyboardUpEventSink :: Key -> IO ()
  , keyboardPressEventSignal :: SignalGen SDLEngine (Signal [Key])
  , keyboardPressEventSink :: Key -> IO ()

  , windowResizeEventSignal :: SignalGen SDLEngine (Signal [V2 Int])
  , windowResizeEventSink :: V2 Int -> IO ()
  }

instance Engine SDLEngine where
  loadImage _ = return $ Image ()

  render engine (Graphics2D element) = render2d engine element
  cleanup _ = Init.quit

  tick engine = do
    mayhaps <- Event.pumpEvents >> Event.pollEvent

    case mayhaps of
      -- Handle the quit event exclusively first to simplify our code
      Just Event.Event { eventPayload = Event.QuitEvent } ->
        return Nothing

      Just Event.Event { .. } ->
        sinkEvent engine eventPayload >>= tick

      Nothing -> return $ Just engine

  mouseMoveSignal = mouseMoveEventSignal
  mouseDownSignal = mouseDownEventSignal
  mouseUpSignal = mouseUpEventSignal
  mouseClickSignal = mouseClickEventSignal

  keyboardDownSignal = keyboardDownEventSignal
  keyboardUpSignal = keyboardUpEventSignal
  keyboardPressSignal = keyboardPressEventSignal

  windowResizeSignal = windowResizeEventSignal

  runningTime _ = fromIntegral <$> Time.ticks
  windowSize SDLEngine { window } = fmap (fmap fromIntegral) . SDL.get $ Video.windowSize window

-- | Creates the default configuration for the engine. You should change the values where necessary.
defaultConfig :: SDLEngineConfig
defaultConfig = SDLEngineConfig
  { windowDimensions = (800, 600)
  , windowIsFullscreen = False
  , windowIsResizable = True
  , windowTitle = "Helm"
  }

-- | Initialises a new engine with default configuration. The engine can then be run later using 'run'.
startup :: IO SDLEngine
startup = startupWith defaultConfig

-- | Initializes a new engine with some configration.
startupWith :: SDLEngineConfig -> IO SDLEngine
startupWith config@SDLEngineConfig{..} = do
  Init.initializeAll

  window <- Video.createWindow (T.pack windowTitle) windowConfig
  renderer <- Video.createRenderer window (-1) rendererConfig

  mouseMoveEvent <- externalMulti
  mouseDownEvent <- externalMulti
  mouseUpEvent <- externalMulti
  mouseClickEvent <- externalMulti
  keyboardDownEvent <- externalMulti
  keyboardUpEvent <- externalMulti
  keyboardPressEvent <- externalMulti
  windowResizeEvent <- externalMulti

  Video.showWindow window

  return SDLEngine
    { window = window
    , renderer = renderer
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
    (w, h) = windowDimensions
    rendererConfig = Video.RendererConfig Video.AcceleratedVSyncRenderer False
    windowConfig = Video.defaultWindow
      { windowInitialSize = V2 (fromIntegral w) (fromIntegral h)
      , windowMode = if windowIsFullscreen
                     then Video.Fullscreen
                     else Video.Windowed
      , windowResizable = windowIsResizable
      }

-- | Renders a 2D element to the engine screen.
render2d :: SDLEngine -> Element -> IO ()
render2d SDLEngine { window, renderer } element = do
  dims <- SDL.get $ Video.windowSize window
  texture <- Renderer.createTexture renderer mode access dims

  Graphics2D.render texture dims element
  Renderer.clear renderer
  Renderer.copy renderer texture Nothing Nothing
  Renderer.destroyTexture texture
  Renderer.present renderer

  where
    mode = Renderer.ARGB8888
    access = Renderer.TextureAccessStreaming

-- | Turns a point containing a vector into a regular vector.
depoint :: Point f a -> f a
depoint (P x) = x

-- | Sinks an SDL event into the Elerea sinks initialized at startup.
-- These sinks then provide an Elerea signal to the subscriptions provided
-- throughout the engine.
sinkEvent :: SDLEngine -> Event.EventPayload -> IO SDLEngine
sinkEvent engine (Event.WindowResizedEvent Event.WindowResizedEventData { .. }) = do
  windowResizeEventSink engine $ fromIntegral <$> windowResizedEventSize

  return engine

sinkEvent engine (Event.MouseMotionEvent Event.MouseMotionEventData { .. }) = do
  mouseMoveEventSink engine $ fromIntegral <$> depoint mouseMotionEventPos

  return engine

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

sinkEvent engine (Event.MouseButtonEvent Event.MouseButtonEventData { .. }) =
  case mouseButtonEventMotion of
    Event.Pressed -> do
      ticks <- Time.ticks
      mouseDownEventSink engine tup

      return engine { lastMousePress = Just (ticks, pos) }

    Event.Released -> do
      mouseUpEventSink engine tup

      -- Weirdly enough, SDL provides a value that says how many clicks there
      -- were, but this value is always set to one even if it's just a regular
      -- mouse up event. Note that here we're defining a click as a mouse up
      -- event being in a very close proximity to a previous mouse down event.
      -- We manually calculate whether this was a click or not.
      case lastMousePress of
        Just (lastTicks, V2 lastX lastY) -> do
          ticks <- Time.ticks

          -- Check that it's a expected amount of time for a click and that the mouse has basically stayed in place
          if ticks - lastTicks < clickMs && (abs (lastX - x) <= clickRadius && abs (lastY - y) <= clickRadius)
          then mouseClickEventSink engine tup
          else return ()

        Nothing -> return ()

      return engine

  where
    SDLEngine { lastMousePress } = engine
    clickMs = 500  -- How long between mouse down/up to recognise clicks
    clickRadius = 1  -- The pixel radius to be considered a click.
    pos@(V2 x y) = depoint mouseButtonEventPos
    tup = (mapMouseButton mouseButtonEventButton, fromIntegral <$> pos)

sinkEvent engine _ = return engine
