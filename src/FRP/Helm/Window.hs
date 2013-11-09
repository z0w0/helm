{-| Contains signals that sample input from the game window. -}
module FRP.Helm.Window (
	-- * Dimensions
	dimensions, width, height
) where

import Control.Applicative
import FRP.Elerea.Simple
import FRP.Helm (Engine(..))
import qualified Graphics.UI.SDL as SDL

{-| The current dimensions of the window. -}
dimensions :: Engine -> SignalGen (Signal (Int, Int))
dimensions (Engine { window }) = effectful $ (\(SDL.Size w h) -> (w, h))<$> SDL.getWindowSize window

{-| The current width of the window. -}
width :: Engine -> SignalGen (Signal Int)
width (Engine { window }) = effectful $ (\(SDL.Size w _) -> w) <$> SDL.getWindowSize window

{-| The current height of the window. -}
height :: Engine -> SignalGen (Signal Int)
height (Engine { window }) = effectful $ (\(SDL.Size _ h) -> h) <$> SDL.getWindowSize window
