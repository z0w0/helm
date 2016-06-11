module Helm.Engine (
  -- * Types
  EngineConfig(..),
  Engine(..),
  -- * Setup
  defaultConfig
) where

import qualified SDL.Video as Video

{-| A data structure describing how to run the engine. -}
data EngineConfig = EngineConfig {
  windowDimensions   :: (Int, Int),
  windowIsFullscreen :: Bool,
  windowIsResizable  :: Bool,
  windowTitle        :: String,
  windowQuitOnClose  :: Bool
}

{-| A data structure describing the game engine's state. -}
data Engine = Engine {
  window         :: Video.Window,
  renderer       :: Video.Renderer,
  engineConfig   :: EngineConfig
}

{-| Creates the default configuration for the engine. You should change the
    values where necessary. -}
defaultConfig :: EngineConfig
defaultConfig = EngineConfig {
  windowDimensions = (800, 600),
  windowIsFullscreen = False,
  windowIsResizable = True,
  windowTitle = "",
  windowQuitOnClose = True
}
