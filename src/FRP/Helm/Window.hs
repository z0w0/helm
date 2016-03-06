{-| Contains signals that sample input from the game window. -}
module FRP.Helm.Window (
  -- * Dimensions
  dimensions,
  width,
  height,
  position
) where

import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Engine
import FRP.Helm.Sample
import FRP.Helm.Signal
import SDL
import qualified SDL.Video as Video
import Linear.V2 (V2(V2))

{-| The current dimensions of the window. -}
dimensions :: Signal (Int, Int)
dimensions =
  Signal $ input >>= getDimensions >>= transfer (pure (0,0)) update
  where
    getDimensions = effectful1 action
    action engine = do
      V2 w h <- SDL.get $ Video.windowSize (window engine)
      return (fromIntegral w, fromIntegral h)

{-| The current position of the window. -}
position :: Signal (Int, Int)
position =
  Signal $ input >>= getPosition >>= transfer (pure (0,0)) update
  where
    getPosition = effectful1 action
    action engine = do
        V2 x y <- Video.getWindowAbsolutePosition (window engine)
        return (fromIntegral x, fromIntegral y)

{-| The current width of the window. -}
width :: Signal Int
width = fst <~ dimensions

{-| The current height of the window. -}
height :: Signal Int
height = snd <~ dimensions
