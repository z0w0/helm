-- | Contains the SDL keyboard mappings.
module Helm.Engine.SDL.Keyboard (mapKey) where

import qualified SDL.Input.Keyboard.Codes as Codes

import           Helm.Engine (Key(..))

-- | Maps an SDL keycode to a Helm key.
mapKey :: Codes.Keycode -> Key
mapKey Codes.KeycodeReturn = ReturnKey
mapKey _ = UnknownKey
