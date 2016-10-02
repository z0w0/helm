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
import Linear.V2 (V2)

import Helm.Engine (Sub(..), Engine(..), MouseButton(..))

-- | Subscribe to mouse movement events and map to a game action.
moves
  :: Engine e
  => (V2 Int -> a)  -- ^ The function to map a mouse position to an action.
  -> Sub e a        -- ^ The mapped subscription.
moves f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> mouseMoveSignal engine

-- | Subscribe to mouse click events and map to a game action.
-- This subscription is for all mouse buttons - you'll need to
-- match over a mouse button if you want to capture a specific one.
--
-- Note that Helm defines a mouse click as a mouse up event which
-- came after a very recent mouse down event in a close radius
-- of the mouse down event.
clicks
  :: Engine e
  => (MouseButton -> V2 Int -> a)  -- ^ The function to map a mouse button and position to an action.
  -> Sub e a                       -- ^ The mapped subscription.
clicks f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap (uncurry f)) <$> mouseClickSignal engine

-- | Subscribe to mouse button down events and map to a game action.
downs
  :: Engine e
  => (MouseButton -> V2 Int -> a)  -- ^ The function to map a mouse button and position to an action.
  -> Sub e a                       -- ^ The mapped subscription.
downs f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap (uncurry f)) <$> mouseDownSignal engine

-- | Subscribe to mouse button up events and map to a game action.
ups
  :: Engine e
  => (MouseButton -> V2 Int -> a)  -- ^ The function to map a mouse button and position to an action.
  -> Sub e a                       -- ^ The mapped subscription.
ups f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap (uncurry f)) <$> mouseUpSignal engine
