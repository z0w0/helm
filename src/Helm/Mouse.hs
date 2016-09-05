{-| Contains subscriptions to events from the mouse. -}
module Helm.Mouse
  (
    -- * Types
    MouseButton(..)
    -- * Subscriptions
  , moves
  , clicks
  , downs
  , ups
  ) where

import FRP.Elerea.Param (input, snapshot)
import Linear.V2 (V2(V2))

import Helm.Engine (Sub(..), Engine(..), MouseButton(..))

moves :: Engine e => (V2 Int -> a) -> Sub e a
moves f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> mouseMoveSignal engine

clicks :: Engine e => (MouseButton -> V2 Int -> a) -> Sub e a
clicks _ = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap (\(b, p) -> f b p)) <$> mouseClickSignal engine

downs :: Engine e => (MouseButton -> V2 Int -> a) -> Sub e a
downs _ = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap (\(b, p) -> f b p)) <$> mouseDownSignal engine

ups :: Engine e => (MouseButton -> V2 Int -> a) -> Sub e a
ups f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap (\(b, p) -> f b p)) <$> mouseUpSignal engine
