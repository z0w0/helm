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
import Control.Monad (foldM, void)
import Control.Monad.Trans.State.Lazy (evalStateT)
import FRP.Elerea.Param (start, embed)

import Helm.Asset (Image)
import Helm.Engine (Cmd(..), Sub(..), GameConfig(..), Engine(..))
import Helm.Graphics

-- | A data structure describing a game's state (that is running under an engine).
data Game e m a = Game
  { gameConfig :: GameConfig e m a  -- ^ The configuration of the game, passed by a user.
  , gameModel :: m                  -- ^ The current game model state.
  , actionSmp :: e -> IO [a]        -- ^ A feedable monad that returns actions from mapped subscriptions.
  }

-- | Prepare the game state from an engine and some game configuration.
prepare :: Engine e => e -> GameConfig e m a -> IO (Game e m a)
prepare engine config = do
  {- The call to 'embed' here is a little bit hacky, but seems necessary
     to get this working. This is because 'start' actually computes the signal
     gen passed to it, and all of our signal gens try to fetch
     the 'input' value within the top layer signal gen (rather than in the
     contained signal). But we haven't sampled with the input value yet, so it'll
     be undefined unless we 'embed'. -}
  smp <- start $ embed (return engine) gen

  return Game
    { gameConfig = config
    , gameModel = fst initialFn
    , actionSmp = smp
    }

  where
    GameConfig { initialFn, subscriptionsFn = Sub gen } = config

-- | Runs a Helm game using an engine and some configuration for a game.
-- An engine should first be initialized separately to Helm, and then passed
-- to this function. Helm is written this way so that library users can
-- choose what backend engine they want to use (and hence Helm is engine-agnostic).
--
-- The best engine to get started with is the SDL implementation of Helm,
-- which is currently bundled with the engine (although it will eventually be moved
-- to its own package). See 'Helm.Engine.SDL.startup' for how
-- to startup the SDL engine, which can then be run by this function.
run :: Engine e => e -> GameConfig e m a -> IO ()
run engine config@GameConfig { initialFn } =
  void $ (prepare engine config >>= stepInitial >>= step engine) `finally` cleanup engine

  where
    Cmd monad = snd initialFn
    stepInitial game@Game { gameModel } = do
      actions <- evalStateT monad engine
      model <- foldM (stepModel engine game) gameModel actions

      return game { gameModel = model }

-- | Step the game state forward.
step :: Engine e => e -> Game e m a -> IO ()
step engine game = do
  mayhaps <- tick engine

  case mayhaps of
    Nothing -> return ()

    Just sunkEngine -> do
      actions <- actionSmp sunkEngine
      model <- foldM (stepModel sunkEngine game) gameModel actions

      render sunkEngine $ viewFn model
      step sunkEngine $ game { gameModel = model }

  where
    Game { actionSmp, gameModel, gameConfig = GameConfig { viewFn } } = game

-- | Step the game model forward with a specific game action.
stepModel :: Engine e => e -> Game e m a -> m -> a -> IO m
stepModel engine game model action =
  evalStateT monad engine >>= foldM (stepModel engine game) upModel

  where
    Game { gameConfig = GameConfig { updateFn } } = game
    (upModel, Cmd monad) = updateFn model action
