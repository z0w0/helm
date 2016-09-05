{-| Contains subscriptions to events from the keyboard. -}
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

presses :: Engine e => (Key -> a) -> Sub e a
presses f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> keyboardPressSignal engine

downs :: Engine e => (Key -> a) -> Sub e a
downs f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> keyboardDownSignal engine

ups :: Engine e => (Key -> a) -> Sub e a
ups f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> keyboardUpSignal engine
