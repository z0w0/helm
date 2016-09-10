-- | Contains the SDL mouse button mappings.
module Helm.Engine.SDL.Mouse (mapMouseButton) where

import qualified SDL.Event as Event

import           Helm.Engine (MouseButton(..))

-- | Map an SDL mouse button to a Helm mouse button.
mapMouseButton :: Event.MouseButton -> MouseButton
mapMouseButton Event.ButtonLeft = LeftButton
mapMouseButton Event.ButtonMiddle = MiddleButton
mapMouseButton Event.ButtonRight = RightButton
mapMouseButton Event.ButtonX1 = X1Button
mapMouseButton Event.ButtonX2 = X2Button
mapMouseButton _ = UnknownButton
