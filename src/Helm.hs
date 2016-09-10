{-| Contains the main functions for interfacing with the engine. -}
module Helm
  (
   -- * Types
   Engine
  ,GameConfig(..)
  ,Cmd(..)
  ,Sub(..)
  ,Graphics(..)
   -- * Engine
  ,run
  ,loadImage
  ,loadSound)
  where

import Control.Exception (finally)
import Control.Monad (foldM, void)
import Control.Monad.Trans.State.Lazy (evalStateT)
import FRP.Elerea.Param (start, embed)

import Helm.Engine (Cmd(..), Sub(..), GameConfig(..), Engine(..))
import Helm.Graphics

{-| A data structure describing a game's state (that is running under an engine). -}
data Game e m a = Game
  { gameConfig :: GameConfig e m a
  , gameModel :: m
  , actionSmp :: e -> IO [a]
  }

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

run :: Engine e => e -> GameConfig e m a -> IO ()
run engine config = void $ (prepare engine config >>= step engine) `finally` cleanup engine

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

stepModel :: Engine e => e -> Game e m a -> m -> a -> IO m
stepModel engine game model action =
  evalStateT monad engine >>= foldM (stepModel engine game) upModel

  where
    Game { gameConfig = GameConfig { updateFn } } = game
    (upModel, Cmd monad) = updateFn model action
