-- | Contains subscriptions to events from the keyboard.
module Helm.Keyboard
  (
    -- * Types
    Key(..)
    -- * Subscriptions
  , presses
  , downs
  , ups
  ) where

import FRP.Elerea.Param (input, snapshot)

import Helm.Engine (Engine(..), Sub(..), Key(..))

-- | Subscribe to keyboard press events and map to a game action.
-- A key press event is produced whenever a key is either released
-- or continously held down.
presses
  :: Engine e
  => (Key -> a)  -- ^ The function to map the key pressed to an action.
  -> Sub e a     -- ^ The mapped subscription.
presses f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> keyboardPressSignal engine

-- | Subscribe to keyboard down events and map to a game action.
downs
  :: Engine e
  => (Key -> a)  -- ^ The function to map the key held down to an action.
  -> Sub e a     -- ^ The mapped subscription.
downs f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> keyboardDownSignal engine

-- | Subscribe to keyboard up events and map to a game action.
ups
  :: Engine e
  => (Key -> a)  -- ^ The function to map the key released to an action.
  -> Sub e a     -- ^ The mapped subscription.
ups f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> keyboardUpSignal engine
