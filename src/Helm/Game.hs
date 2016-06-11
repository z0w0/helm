module Helm.Game (
  -- * Types
  GameConfig(..),
  Game(..)
) where

import Control.Concurrent.STM (TQueue)

import Helm.Cmd (Cmd)
import Helm.Engine (Engine)
import Helm.Render (Render)
import Helm.Sub (Sub)

{-| Describes how to run a game. -}
data GameConfig m a = GameConfig {
  initialFn          :: (m, Cmd a),
  updateFn           :: m -> a -> (m, Cmd a),
  subscriptionsFn    :: Sub a,
  viewFn             :: m -> Render ()
}

{-| Describes a game's state. -}
data Game m a = Game {
  gameConfig       :: GameConfig m a,
  gameModel        :: m,
  actionSmp        :: Engine -> IO [a],
  actionQueue      :: TQueue a
}
