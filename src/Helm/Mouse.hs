{-| Contains subscriptions to events from the mouse. -}
module Helm.Mouse
(
  -- * Subscriptions
  moves,
  clicks,
  downs,
  ups,
  buttonClicks,
  buttonDowns,
  buttonUps
) where

import Linear.V2 (V2(V2))
import Helm (Sub(..))
import SDL.Input.Mouse (MouseButton(ButtonLeft))

moves :: (V2 Int -> a) -> Sub a
moves _ = Sub $ return $ return []

buttonClicks :: MouseButton -> (V2 Int -> a) -> Sub a
buttonClicks _ _ = Sub $ return $ return []

buttonDowns :: MouseButton -> (V2 Int -> a) -> Sub a
buttonDowns _ _ = Sub $ return $ return []

buttonUps :: MouseButton -> (V2 Int -> a) -> Sub a
buttonUps _ _ = Sub $ return $ return []

clicks :: (V2 Int -> a) -> Sub a
clicks = buttonClicks ButtonLeft

downs :: (V2 Int -> a) -> Sub a
downs = buttonDowns ButtonLeft

ups :: (V2 Int -> a) -> Sub a
ups = buttonUps ButtonLeft
