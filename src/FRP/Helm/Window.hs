{-| Contains signals that sample input from the game window. -}
module FRP.Helm.Window (
	-- * Dimensions
	dimensions, width, height
) where

import Foreign.Marshal.Alloc
import Foreign.Storable
import FRP.Elerea.Simple
import FRP.Helm (Engine(..))
import FRP.Helm.Utilities
import qualified Graphics.UI.SDL as SDL

{-| The current dimensions of the window. -}
dimensions :: Engine -> SignalGen (Signal (Int, Int))
dimensions (Engine { window }) = effectful $ alloca $ \wptr -> alloca $ \hptr -> do
		SDL.getWindowSize window wptr hptr

		w <- peek wptr
		h <- peek hptr

		return (fromIntegral w, fromIntegral h)
{-| The current width of the window. -}
width :: Engine -> SignalGen (Signal Int)
width engine = (\(w, _) -> w) <~ dimensions engine

{-| The current height of the window. -}
height :: Engine -> SignalGen (Signal Int)
height engine = (\(_, h) -> h) <~ dimensions engine
