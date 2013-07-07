{-# OPTIONS_HADDOCK hide #-}

module FRP.Helm.Internal (keyState) where

import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL

-- FIXME: This entire hacky module can be removed if a non-hacky SDL_GetKeyState can be bound.
-- I'm sorry, I really am.

{-# NOINLINE keyState #-}
keyState :: IORef (Map.Map SDL.SDLKey Bool)
keyState = unsafePerformIO $ newIORef Map.empty
