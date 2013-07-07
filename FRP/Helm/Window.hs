module FRP.Helm.Window (dimensions, width, height) where

import Control.Applicative
import FRP.Elerea.Simple
import qualified Graphics.UI.SDL as SDL

-- |The current dimensions of the window.
dimensions :: SignalGen (Signal (Int, Int))
dimensions = effectful $ (\s -> (SDL.surfaceGetWidth s, SDL.surfaceGetHeight s)) <$> SDL.getVideoSurface

-- |The current width of the window.
width :: SignalGen (Signal Int)
width = effectful $ SDL.surfaceGetWidth <$> SDL.getVideoSurface

-- |The current height of the window.
height :: SignalGen (Signal Int)
height = effectful $ SDL.surfaceGetHeight <$> SDL.getVideoSurface
