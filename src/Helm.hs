-- | Contains the main functions for interfacing with the engine.
-- This can be thought of Helm's own Prelude.
module Helm
  (
    -- * Types
    Cmd(..)
  , Engine
  , GameLifecycle(..)
  , GameConfig(..)
  , Graphics(..)
  , Image
  , FPSLimit(..)
  , Sub(..)
    -- * Engine
  , run
  , defaultConfig
  ) where

import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (foldM, void, (>=>))
import Control.Monad.Trans.State.Lazy (runStateT)
import FRP.Elerea.Param (start, embed)

import Helm.Asset (Image)
import Helm.Engine (Cmd(..), Sub(..), Game(..), GameConfig(..), GameLifecycle(..), Engine(..), FPSLimit(..), defaultConfig)
import Helm.Graphics

-- | The context of an engine running a game.
-- This is used to track the connection of an engine's state to a game state.
data EngineContext e m a = EngineContext e (Game e m a)

-- | Runs a Helm game using an engine and some configuration for a game.
-- An engine should first be initialized separately to Helm, and then passed
-- to this function. Helm is written this way so that library users can
-- choose what backend engine they want to use (and hence Helm is engine-agnostic).
--
-- The best engine to get started with is the SDL implementation of Helm,
-- which is currently bundled with the engine (although it will eventually be moved
-- to its own package). See 'Helm.Engine.SDL.startup' for how
-- to startup the SDL engine, which can then be run by this function.
run
  :: Engine e
  => e                 -- ^ The engine to use to run the game.
  -> GameConfig
  -> GameLifecycle e m a  -- ^ The configuration for running the game.
  -> IO ()             -- ^ An IO monad that blocks the main thread until the engine quits.
run engine config lifecycle@GameLifecycle { initialFn, subscriptionsFn = Sub sigGen } = void $ do
  {- The call to 'embed' here is a little bit hacky, but seems necessary
     to get this working. This is because 'start' actually computes the signal
     gen passed to it, and all of our signal gens try to fetch
     the 'input' value within the top layer signal gen (rather than in the
     contained signal). But we haven't sampled with the input value yet, so it'll
     be undefined unless we 'embed'. -}
  smp <- start $ embed (return engine) sigGen

  -- Setup the initial engine context and perform the initial game step
  ctx@(EngineContext engine_ _) <- flip stepCmd (snd initialFn) $ EngineContext engine Game
      { gameConfig = config
      , gameLifecycle = lifecycle
      , gameModel = fst initialFn
      , dirtyModel = True
      , actionSmp = smp
      , lastRender = 0
      , updateCount = 0
      }

  step ctx `finally` cleanup engine_

-- | Step the engine context forward.
step
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> IO (Maybe (EngineContext e m a))  -- ^ An IO monad that produces the stepped engine context.
step = updateStep >=> maybe (return Nothing) (renderStep >=> delayWithinFPSLimit >=> step)

-- | Continiuslu steps engine context with processing all actions while limit is not reached or Nothing returned in case engine quits.
updateStep
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> IO (Maybe (EngineContext e m a))  -- ^ An IO monad that produces the engine context stepped with maximum allowed ticks and action.
updateStep = stepTick >=> maybe (return Nothing) continueUpdateWithinLimit

-- | Proceeed to the next updateStep if updateCount is not reached yet, returns Nothing in case engine quits.
continueUpdateWithinLimit
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> IO (Maybe (EngineContext e m a))  -- ^ An IO monad that produces the engine context stepped with maximum allowed ticks and action.
continueUpdateWithinLimit context = if updateCount >= updateLimit
                              then return (Just (EngineContext engine game { updateCount = 0 }))
                              else updateStep (EngineContext engine game { updateCount = updateCount + 1 })
  where (EngineContext engine game@Game { gameConfig = GameConfig { updateLimit }, updateCount }) = context

-- | Step the engine context forward with all game actions available, returns Nothing in case engine quits.
stepTick
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> IO (Maybe (EngineContext e m a))  -- ^ An IO monad that produces the engine context stepped with the action.
stepTick (EngineContext engine game) = tick engine >>= maybe (return Nothing) (\engine_ -> Just <$> (stepActions game engine_))

-- | Step the engine forward with all game actions available.
stepActions :: Engine e
  => Game e m a                -- ^ The engine context to step forward.
  -> e                         -- ^ The action to step the engine context with.
  -> IO (EngineContext e m a)  -- ^ An IO monad that produces the engine context stepped with all actions available.
stepActions game@Game { actionSmp } engine = (actionSmp engine) >>= foldM stepAction (EngineContext engine game)

-- | Step the engine context forward with a specific game action.
stepAction
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> a                         -- ^ The action to step the engine context with.
  -> IO (EngineContext e m a)  -- ^ An IO monad that produces the engine context stepped with the action.
stepAction (EngineContext engine game@Game { gameModel, gameLifecycle = GameLifecycle { updateFn } }) action =
  stepCmd ctx cmd

  where
    (updatedModel, cmd) = updateFn gameModel action

    -- Mark the game as dirty and adjust the new game model
    ctx = EngineContext engine $ game
      { dirtyModel = True
      , gameModel = updatedModel
      }

-- | Step the engine context forward with a specific command.
-- This will recursively call 'stepAction' with any actions
-- that are produced by the command.
stepCmd
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> Cmd e a                   -- ^ The command to step the engine context with.
  -> IO (EngineContext e m a)  -- ^ An IO monad that produces the engine context stepped with the command.
stepCmd (EngineContext engine game) (Cmd monad) = do
  (actions, engine_) <- runStateT monad engine

  -- Step any actions returned from the command
  foldM stepAction (EngineContext engine_ game) actions

-- | Renders model if needed
renderStep :: Engine e => EngineContext e m a -> IO (EngineContext e m a)
renderStep = skipWhenDirty Helm.render

-- | Throttle operation based on GameModel dirtiness
skipWhenDirty :: Engine e => (EngineContext e m a -> IO (EngineContext e m a)) -> EngineContext e m a ->  IO (EngineContext e m a)
skipWhenDirty operation context@(EngineContext _ game) = if (dirtyModel game)
                                                         then operation context
                                                         else return context

-- | Renders context, resets game's model dirtiness
render :: Engine e => EngineContext e m a -> IO (EngineContext e m a)
render (EngineContext engine game@Game { gameModel, gameLifecycle = GameLifecycle { viewFn } }) = do
  Helm.Engine.render engine $ viewFn gameModel
  return (EngineContext engine game { dirtyModel = False })

-- | Delays thread to satisfy FPSLimit requirement and updates when last frame was renedered.
delayWithinFPSLimit :: Engine e => EngineContext e m a -> IO (EngineContext e m a)
delayWithinFPSLimit context = do
  runningTime engine >>= delayIfNeeded fpsLimit lastRender
  runningTime engine >>= \currentTime -> return (EngineContext engine game { lastRender = currentTime })
  where EngineContext engine game@Game { gameConfig = GameConfig { fpsLimit = fpsLimit }, lastRender } = context

-- | Delays thread to satisfy FPSLimit requirement.
delayIfNeeded
  :: FPSLimit -- ^ FPS limit setting.
  -> Double   -- ^ Last time when frame was rendered.
  -> Double   -- ^ Current time.
  -> IO ()    -- ^ An IO monad that delays the main thread to ensure that frames are not rendered faster than limit allows.
delayIfNeeded Unlimited _ _ = return ()
delayIfNeeded (Limited fpsLimit) lastRender currentTime =  do
  if delay > 0
  then threadDelay delay
  else putStrLn "Warning: FPS degradation. You may want to tune your update or FPS limits."
  where
    delay = ceiling $ 1000*(currentTime - lastRender) - (fromIntegral fpsLimit)
