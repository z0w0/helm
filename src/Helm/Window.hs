{-| Contains signals that sample input from the game window. -}
module Helm.Window
  (
    -- * Commands
    size
    -- * Subscriptions
  , resizes
  ) where

import Control.Monad.State (get)
import Control.Monad.IO.Class (liftIO)

import FRP.Elerea.Param (input, snapshot)
import Linear.V2 (V2)

import Helm.Engine (Engine(..), Cmd(..), Sub(..))

size :: Engine e => (V2 Int -> a) -> Cmd e a
size f = Cmd $ do
  engine <- get
  sized <- liftIO $ f <$> windowSize engine

  return [sized]

resizes :: Engine e => (V2 Int -> a) -> Sub e a
resizes f = Sub $ do
  engine <- input >>= snapshot

  fmap (map f) <$> windowResizeSignal engine
