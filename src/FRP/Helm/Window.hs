{-| Contains signals that sample input from the game window. -}
module FRP.Helm.Window (
  -- * Dimensions
  dimensions,
  width,
  height,
  position
) where

import Control.Applicative (pure)
import Foreign.Marshal.Alloc
import Foreign.Storable
import FRP.Elerea.Simple hiding (Signal)
import FRP.Helm.Engine
import FRP.Helm.Sample
import FRP.Helm.Signal
import qualified Graphics.UI.SDL as SDL

{-| The current dimensions of the window. -}
dimensions :: Engine -> Signal (Int, Int)
dimensions (Engine { window }) =
  Signal $ getDimensions >>= transfer (pure (0,0)) update
  where
    getDimensions = effectful $ alloca $ \wptr -> alloca $ \hptr -> do
	    SDL.getWindowSize window wptr hptr

	    w <- peek wptr
	    h <- peek hptr

	    return (fromIntegral w, fromIntegral h)

{-| The current position of the window. -}
position :: Engine -> Signal (Int, Int)
position (Engine { window }) =
  Signal $ getPosition >>= transfer (pure (0,0)) update
  where
    getPosition = effectful $ alloca $ \xptr -> alloca $ \yptr -> do
	    SDL.getWindowPosition window xptr yptr

	    x <- peek xptr
	    y <- peek yptr

	    return (fromIntegral x, fromIntegral y)

{-| The current width of the window. -}
width :: Engine -> Signal Int
width engine = fst <~ dimensions engine

{-| The current height of the window. -}
height :: Engine -> Signal Int
height engine = snd <~ dimensions engine
