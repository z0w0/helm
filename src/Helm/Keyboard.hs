{-| Contains subscriptions to events from the keyboard. -}
module Helm.Keyboard (
  -- * Subscriptions
  presses,
  downs,
  ups
) where

import SDL.Input.Keyboard.Codes (Keycode)
import Helm (Sub(..))

presses :: (Keycode -> a) -> Sub a
presses _ = Sub $ return $ return []

downs :: (Keycode -> a) -> Sub a
downs _ = Sub $ return $ return []

ups :: (Keycode -> a) -> Sub a
ups _ = Sub $ return $ return []
