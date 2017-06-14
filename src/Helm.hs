-- | Contains the main functions for interfacing with the engine.
-- This can be thought of Helm's own Prelude.
module Helm
  (
    -- * Types
    Cmd(..)
  , Engine
  , GameConfig(..)
  , Graphics(..)
  , Image
  , Sub(..)
    -- * Engine
  , run
  ) where

import Control.Exception (finally)
import Control.Monad (foldM, void, when)
import Control.Monad.Trans.State.Lazy (runStateT)
import FRP.Elerea.Param (start, embed)

import Helm.Asset (Image)
import Helm.Engine (Cmd(..), Sub(..), Game(..), GameConfig(..), Engine(..))
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
  -> GameConfig e m a  -- ^ The configuration for running the game.
  -> IO ()             -- ^ An IO monad that blocks the main thread until the engine quits.
run engine config@GameConfig { initialFn, subscriptionsFn = Sub sigGen } = void $ do
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
      , gameModel = fst initialFn
      , dirtyModel = True
      , actionSmp = smp
      }

  step ctx `finally` cleanup engine_

-- | Step the engine context forward.
step
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> IO (EngineContext e m a)  -- ^ An IO monad that produces the stepped engine context.
step ctx@(EngineContext engine game) = do
  -- Tick the engine to pump the signal sinks
  mayhaps <- tick engine

  case mayhaps of
    -- If nothing was returned, stop the step loop
    Nothing -> return ctx

    Just engine_ -> do
      EngineContext engine__ game_ <- actionSmp engine_ >>= foldM stepAction (EngineContext engine_ game)

      -- Render the game if game model has been changed this step
      when (dirtyModel game_) $ render engine__ $ viewFn $ gameModel game_

      -- Keep the loop going
      step $ EngineContext engine__ $ game_ { dirtyModel = False }

  where
    Game { actionSmp, gameConfig = GameConfig { viewFn } } = game

-- | Step the engine context forward with a specific game action.
stepAction
  :: Engine e
  => EngineContext e m a       -- ^ The engine context to step forward.
  -> a                         -- ^ The action to step the engine context with.
  -> IO (EngineContext e m a)  -- ^ An IO monad that produces the engine context stepped with the action.
stepAction (EngineContext engine game@Game { gameModel, gameConfig = GameConfig { updateFn } }) action =
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

