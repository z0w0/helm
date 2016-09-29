{-# LANGUAGE TypeFamilies #-}
-- | Contains the SDL implementation of Helm.
module Helm.Engine.SDL
  (
    -- * Types
    SDLEngine
  , SDLEngineConfig(..)
    -- * Startup
  , defaultConfig
  , startup
  , startupWith
    -- * Asset Loading
  , withImage
  ) where

import           Control.Monad (when)
import qualified Data.Text as T

import           FRP.Elerea.Param
import           Linear.Affine (Point(P))
import           Linear.Metric (distance)
import           Linear.V2 (V2(V2))

import qualified SDL
import qualified SDL.Event as Event
import qualified SDL.Init as Init
import           SDL.Input.Keyboard (Keysym(..))
import qualified SDL.Time as Time
import qualified SDL.Video as Video
import           SDL.Video (WindowConfig(..))
import qualified SDL.Video.Renderer as Renderer

import           Helm.Engine (Engine(..))
import           Helm.Engine.SDL.Asset (withImage)
import           Helm.Engine.SDL.Engine (SDLEngine(..), SDLEngineConfig(..))
import qualified Helm.Engine.SDL.Graphics2D as Graphics2D
import           Helm.Engine.SDL.Keyboard (mapKey)
import           Helm.Engine.SDL.Mouse (mapMouseButton)
import           Helm.Graphics (Graphics(..))
import           Helm.Graphics2D (Collage)

-- FIXME: Find a nice and easy way to have this instance with the SDLEngine type.
-- Can't avoid the orphan instance without dependency hell right now.
instance Engine SDLEngine where
  render engine (Graphics2D coll) = render2d engine coll
  cleanup SDLEngine { window, renderer, texture } = do
    Renderer.destroyTexture texture
    Video.destroyWindow window
    Video.destroyRenderer renderer
    Init.quit

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
  { windowDimensions = V2 800 600
  , windowIsFullscreen = False
  , windowIsResizable = True
  , windowTitle = "Helm"
  }

-- | Initialises a new engine with default configuration. The engine can then be run later using 'run'.
startup :: IO SDLEngine
startup = startupWith defaultConfig

-- | Prepare a texture for streamed rendering based of a window size.
prepTexture :: V2 Int -> Video.Renderer -> IO Renderer.Texture
prepTexture dims renderer =
  Renderer.createTexture renderer mode access $ fromIntegral <$> dims

  where
    mode = Renderer.ARGB8888
    access = Renderer.TextureAccessStreaming

-- | Initializes a new engine with some configration, ready to be 'run'.
startupWith :: SDLEngineConfig -> IO SDLEngine
startupWith config@SDLEngineConfig { .. } = do
  Init.initializeAll

  window <- Video.createWindow (T.pack windowTitle) windowConfig
  renderer <- Video.createRenderer window (-1) rendererConfig
  texture <- prepTexture windowDimensions renderer

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
render2d :: SDLEngine -> Collage SDLEngine -> IO ()
render2d SDLEngine { window, renderer, texture } element = do
  dims <- SDL.get $ Video.windowSize window

  Graphics2D.render texture dims element
  Renderer.clear renderer
  Renderer.copy renderer texture Nothing Nothing
  Renderer.present renderer

-- | Turns a point containing a vector into a regular vector.
depoint :: Point f a -> f a
depoint (P x) = x

-- | Sinks an SDL event into the Elerea sinks initialized at startup.
-- These sinks then provide an Elerea signal to the subscriptions provided
-- throughout the engine.
sinkEvent :: SDLEngine -> Event.EventPayload -> IO SDLEngine
sinkEvent engine (Event.WindowResizedEvent Event.WindowResizedEventData { .. }) = do
  windowResizeEventSink engine dims
  Renderer.destroyTexture texture

  resized <- prepTexture dims renderer

  return engine { texture = resized }

  where
    dims = fromIntegral <$> windowResizedEventSize
    SDLEngine { texture, renderer } = engine

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
    clickRadius = 1  -- The pixel radius to be considered a click.
    pos = depoint mouseButtonEventPos
    dubPos = fromIntegral <$> pos
    tup = (mapMouseButton mouseButtonEventButton, fromIntegral <$> pos)

sinkEvent engine _ = return engine
