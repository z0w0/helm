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

import           Control.Exception (finally)
import           Control.Monad (foldM, void)
import           Control.Monad.Trans.State.Lazy (evalStateT)
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
import           Helm.Engine (GameConfig(..), Cmd(..), Sub(..),
                              Engine(..), Key, MouseButton)
import           Helm.Graphics (Graphics(..))
import           Helm.Graphics2D (Element)
import           Helm.Engine.SDL.Keyboard (mapKey)
import           Helm.Engine.SDL.Mouse (mapMouseButton)
import qualified Helm.Engine.SDL.Graphics2D as Graphics2D

{-| A data structure describing how to run the engine. -}
data SDLEngineConfig = SDLEngineConfig
  { windowDimensions :: (Int, Int)
  , windowIsFullscreen :: Bool
  , windowIsResizable :: Bool
  , windowTitle :: String
  }

{-| A data structure describing the game engine's state. -}
data SDLEngine = SDLEngine
  { window :: Video.Window
  , renderer :: Video.Renderer
  , engineConfig :: SDLEngineConfig

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

{-| A data structure describing a game's state (that is running under an engine). -}
data SDLGame m a = SDLGame
  { gameConfig :: GameConfig SDLEngine m a
  , gameModel :: m
  , running :: Bool
  , actionSmp :: SDLEngine -> IO [a]
  , lastMousePress :: Maybe (Word32, V2 Int32)
  }

instance Engine SDLEngine where
  loadImage _ = return $ Image ()
  loadSound _ = return $ Sound ()

  mouseMoveSignal = mouseMoveEventSignal
  mouseDownSignal = mouseDownEventSignal
  mouseUpSignal = mouseUpEventSignal
  mouseClickSignal = mouseClickEventSignal

  keyboardDownSignal = keyboardDownEventSignal
  keyboardUpSignal = keyboardUpEventSignal
  keyboardPressSignal = keyboardPressEventSignal

  windowResizeSignal = windowResizeEventSignal

  runningTime _ = fromIntegral <$> Time.ticks
  windowSize SDLEngine { window } =
    fmap (fmap fromIntegral) . SDL.get $ Video.windowSize window

  run engine config =
    void $ (prepare engine config >>= step engine) `finally` Init.quit

{-| Creates the default configuration for the engine. You should change the
    values where necessary. -}
defaultConfig :: SDLEngineConfig
defaultConfig = SDLEngineConfig
  { windowDimensions = (800, 600)
  , windowIsFullscreen = False
  , windowIsResizable = True
  , windowTitle = "Helm"
  }

{-| Initialises a new engine with default configuration.
    The engine can then be run later using 'run'. -}
startup :: IO SDLEngine
startup = startupWith defaultConfig

{-| Initializes a new engine with some configration. -}
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

step :: SDLEngine -> SDLGame m a -> IO (SDLGame m a)
step engine game@SDLGame{actionSmp,gameModel,gameConfig = GameConfig{viewFn}} = do
  sunkGame <- sinkEvents engine game

  if running sunkGame
  then do
    actions <- actionSmp engine
    model <- foldM (stepModel engine game) gameModel actions

    render engine $ viewFn model
    step engine $ sunkGame { gameModel = model }
  else return sunkGame

stepModel :: SDLEngine -> SDLGame m a -> m -> a -> IO m
stepModel engine game@SDLGame { gameConfig = GameConfig { updateFn } } model action =
  evalStateT monad engine >>= foldM (stepModel engine game) model

  where
    (model, Cmd monad) = updateFn model action

prepare :: SDLEngine -> GameConfig SDLEngine m a -> IO (SDLGame m a)
prepare engine config@GameConfig { initialFn, subscriptionsFn = Sub gen } = do
  {- The call to 'embed' here is a little bit hacky, but seems necessary
     to get this working. This is because 'start' actually computes the signal
     gen passed to it, and all of our signal gens try to fetch
     the 'input' value within the top layer signal gen (rather than in the
     contained signal). But we haven't sampled with the input value yet, so it'll
     be undefined unless we 'embed'. -}
  smp <- start $ embed (return engine) gen

  return SDLGame
    { gameConfig = config
    , gameModel = fst initialFn
    , running = True
    , actionSmp = smp
    , lastMousePress = Nothing
    }

render :: SDLEngine -> Graphics -> IO ()
render engine (Graphics2D element) = render2d engine element

render2d :: SDLEngine -> Element -> IO ()
render2d SDLEngine{window,renderer} element = do
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

sinkEvents :: SDLEngine -> SDLGame m a -> IO (SDLGame m a)
sinkEvents engine game = do
  mayhaps <- Event.pumpEvents >> Event.pollEvent

  case mayhaps of
      -- Handle the quit event exclusively first to simplify our code
      Just Event.Event { eventPayload = Event.QuitEvent } ->
        return game { running = False }

      Just Event.Event { .. } ->
        sinkEvent engine game eventPayload >>= sinkEvents engine

      Nothing -> return game

depoint :: Point f a -> (f a)
depoint (P x) = x

sinkEvent :: SDLEngine -> SDLGame m a -> Event.EventPayload -> IO (SDLGame m a)
sinkEvent engine game (Event.WindowResizedEvent Event.WindowResizedEventData { .. }) = do
  windowResizeEventSink engine $ fromIntegral <$> windowResizedEventSize

  return game

sinkEvent engine game (Event.MouseMotionEvent Event.MouseMotionEventData { .. }) = do
  mouseMoveEventSink engine $ fromIntegral <$> depoint mouseMotionEventPos

  return game

sinkEvent engine game (Event.KeyboardEvent Event.KeyboardEventData { .. }) = do
  case keyboardEventKeyMotion of
    Event.Pressed -> do
      keyboardDownEventSink engine key

      if keyboardEventRepeat
      then keyboardPressEventSink engine key >> return game
      else return game

    Event.Released -> do
      keyboardUpEventSink engine key
      keyboardPressEventSink engine key

      return game

  where
    Keysym { .. } = keyboardEventKeysym
    key = mapKey keysymKeycode

sinkEvent engine game (Event.MouseButtonEvent Event.MouseButtonEventData { .. }) = do
  case mouseButtonEventMotion of
    Event.Pressed -> do
      ticks <- Time.ticks
      mouseDownEventSink engine tup

      return game { lastMousePress = Just (ticks, pos) }

    Event.Released -> do
      mouseUpEventSink engine tup

      {- Weirdly enough, SDL provides a value that says how many clicks there
         were, but this value is always set to one even if it's just a regular
         mouse up event. Note that here we're defining a click as a mouse up
         event being in a very close proximity to a previous mouse down event.
         We manually calculate whether this was a click or not. -}
      case lastMousePress of
        Just (lastTicks, (V2 lastX lastY)) -> do
          ticks <- Time.ticks

          -- Check that it's a expected amount of time for a click and that the mouse has basically stayed in place
          if ticks - lastTicks < clickMs && (abs (lastX - x) <= clickRadius && abs (lastY - y) <= clickRadius)
          then mouseClickEventSink engine tup
          else return ()

        Nothing -> return ()

      return game

  where
    SDLGame { lastMousePress } = game
    clickMs = 500  -- How long between mouse down/up to recognise clicks
    clickRadius = 1  -- The pixel radius to be considered a click.
    pos@(V2 x y) = depoint mouseButtonEventPos
    tup = (mapMouseButton mouseButtonEventButton, fromIntegral <$> pos)

sinkEvent _ game _ = return game
