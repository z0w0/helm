{-| Contains the main functions for interfacing with the engine. -}
module Helm (
  -- * Types
  Render(..),
  Engine,
  EngineConfig(..),
  Game,
  GameConfig(..),
  Cmd(..),
  Sub(..),
  -- * Engine
  startup,
  startupWith,
  run
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (foldM, forM_, void)
import Control.Monad.Trans.State (evalStateT)
import FRP.Elerea.Param
import Linear.V2 (V2(V2))

import SDL.Event
import SDL.Video (WindowConfig(..))
import qualified SDL.Init as Init
import qualified SDL.Video as Video
import qualified Data.Text as T

import Helm.Cmd (Cmd(..))
import Helm.Engine
import Helm.Game
import Helm.Render (Render(..))
import Helm.Sub (Sub(..))

{-| Initialises a new engine with default configuration.
    The engine can then be run later using 'run'. -}
startup :: IO Engine
startup = startupWith defaultConfig

{-| Initializes a new engine with some configration. -}
startupWith :: EngineConfig -> IO Engine
startupWith config@(EngineConfig { .. }) = do
  Init.initializeAll

  window <- Video.createWindow (T.pack windowTitle) windowConfig
  renderer <- Video.createRenderer window (-1) rendererConfig

  Video.showWindow window

  return Engine {
    window         = window,
    renderer       = renderer,
    engineConfig   = config
  }

  where
    (w, h) = windowDimensions
    rendererConfig = Video.RendererConfig Video.AcceleratedVSyncRenderer False
    windowConfig = Video.defaultWindow {
      windowInitialSize = V2 (fromIntegral w) (fromIntegral h),
      windowMode = if windowIsFullscreen then Video.Fullscreen else Video.Windowed,
      windowResizable = windowIsResizable
    }

prepare :: GameConfig m a -> IO (Game m a)
prepare config = do
  smp <- start $ signalGen $ subscriptionsFn config
  queue <- newTQueueIO

  return Game {
    gameConfig  = config,
    gameModel   = fst $ initialFn config,
    actionSmp   = smp,
    actionQueue = queue
  }

  where
    signalGen (Sub gen) = gen

dequeueCmds :: Game m a -> IO [a]
dequeueCmds game = atomically dequeue
  where
    dequeue = do
      x <- tryReadTQueue (actionQueue game)

      case x of
        Nothing -> return []
        Just action -> do
          xs <- dequeue

          return $ action : xs

queueCmd :: Engine -> Game m a -> Cmd a -> IO ()
queueCmd engine game (Cmd monad) = void $ forkIO $ do
  actions <- evalStateT monad engine

  atomically $ forM_ actions (writeTQueue $ actionQueue game)

run :: Engine -> GameConfig m a -> IO ()
run engine config = do
  game <- prepare config

  queueCmd engine game $ snd $ initialFn config
  tick engine game `finally` Init.quit

tick :: Engine -> Game m a -> IO ()
tick engine game = do
  actions <- (++) <$> actionSmp game engine <*> dequeueCmds game
  stepped <- foldM (step engine game) (gameModel game) actions

  render engine game stepped
  tick engine $ game { gameModel = stepped }

step :: Engine -> Game m a -> m -> a -> IO m
step engine game model action = do
  queueCmd engine game $ snd result

  return $ fst result

  where
    result = (updateFn $ gameConfig game) model action

render :: Engine -> Game m a -> m -> IO ()
render engine game model = evalStateT monad engine >> return ()
  where
    Game { gameConfig = GameConfig { viewFn } } = game
    Render monad = viewFn model
