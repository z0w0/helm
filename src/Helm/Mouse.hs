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
  , buttonClicks
  , buttonDowns
  , buttonUps
  ) where

import FRP.Elerea.Param (input, snapshot)
import Linear.V2 (V2(V2))

import Helm.Engine (Sub(..), Engine(..), MouseButton(..))

moves :: Engine e => (V2 Int -> a) -> Sub e a
moves f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> mouseMoveSignal engine

buttonClicks :: Engine e => MouseButton -> (V2 Int -> a) -> Sub e a
buttonClicks _ _ = Sub $ return $ return []

buttonDowns :: Engine e => MouseButton -> (V2 Int -> a) -> Sub e a
buttonDowns _ _ = Sub $ return $ return []

buttonUps :: Engine e => MouseButton -> (V2 Int -> a) -> Sub e a
buttonUps _ _ = Sub $ return $ return []

clicks :: Engine e => (V2 Int -> a) -> Sub e a
clicks = buttonClicks LeftButton

downs :: Engine e => (V2 Int -> a) -> Sub e a
downs = buttonDowns LeftButton

ups :: Engine e => (V2 Int -> a) -> Sub e a
ups = buttonUps LeftButton
