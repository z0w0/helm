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
import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Engine
import FRP.Helm.Sample
import FRP.Helm.Signal
import qualified Graphics.UI.SDL as SDL

{-| The current dimensions of the window. -}
dimensions :: Signal (Int, Int)
dimensions =
  Signal $ input >>= getDimensions >>= transfer (pure (0,0)) update
  where
    getDimensions = effectful1 action
    action engine = alloca $ \wptr -> alloca $ \hptr -> do
	  SDL.getWindowSize (window engine) wptr hptr

	  w <- peek wptr
	  h <- peek hptr

	  return (fromIntegral w, fromIntegral h)

{-| The current position of the window. -}
position :: Signal (Int, Int)
position =
  Signal $ input >>= getPosition >>= transfer (pure (0,0)) update
  where
    getPosition = effectful1 action
    action engine = alloca $ \xptr -> alloca $ \yptr -> do
	  SDL.getWindowPosition (window engine) xptr yptr

	  x <- peek xptr
	  y <- peek yptr

	  return (fromIntegral x, fromIntegral y)

{-| The current width of the window. -}
width :: Signal Int
width = fst <~ dimensions

{-| The current height of the window. -}
height :: Signal Int
height = snd <~ dimensions
