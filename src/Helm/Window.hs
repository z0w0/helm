-- | Contains signals that sample input from the game window.
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

-- | Map the game window size to a game action.
size
  :: Engine e
  => (V2 Int -> a)  -- ^ The function to map the window size to an action.
  -> Cmd e a        -- ^ The mapped command.
size f = Cmd $ do
  engine <- get
  sized <- liftIO $ f <$> windowSize engine

  return [sized]

-- | Subscribe to the resize events from the game window and map to a game action.
resizes
  :: Engine e
  => (V2 Int -> a)  -- ^ The function to map the changed window size to an action.
  -> Sub e a        -- ^ The mapped subscription.
resizes f = Sub $ do
  engine <- input >>= snapshot

  fmap (map f) <$> windowResizeSignal engine
