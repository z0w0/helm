-- | Contains the SDL engine types.
module Helm.Engine.SDL.Engine
  (
    -- * Types
    SDLEngine(..)
  , SDLEngineConfig(..)
  ) where

import           Data.Word (Word32)

import           FRP.Elerea.Param (Signal, SignalGen)
import           Linear.V2 (V2)
import qualified SDL.Video as Video
import qualified SDL.Video.Renderer as Renderer

import           Helm.Engine (MouseButton, Key)

-- | Represents the configuration to run the SDL engine with.
-- Use 'defaultConfig' and then only change the necessary fields.
data SDLEngineConfig = SDLEngineConfig
  { windowDimensions :: V2 Int
  , windowIsFullscreen :: !Bool
  , windowIsResizable :: !Bool
  , windowTitle :: !String
  }

-- | Represents the SDL engine's internal state.
data SDLEngine = SDLEngine
  { window :: Video.Window
  , renderer :: Video.Renderer
  , texture :: !Renderer.Texture
  , engineConfig :: SDLEngineConfig
  , lastMousePress :: Maybe (Word32, V2 Double)

  , mouseMoveEventSignal :: SignalGen SDLEngine (Signal [V2 Int])
  , mouseMoveEventSink :: V2 Int -> IO ()
  , mouseDownEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])
  , mouseDownEventSink :: (MouseButton, V2 Int) -> IO ()
  , mouseUpEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])
  , mouseUpEventSink :: (MouseButton, V2 Int) -> IO ()
  , mouseClickEventSignal :: SignalGen SDLEngine (Signal [(MouseButton, V2 Int)])
  , mouseClickEventSink :: (MouseButton, V2 Int) -> IO ()

  , keyboardDownEventSignal :: SignalGen SDLEngine (Signal [Key])
  , keyboardDownEventSink :: Key -> IO ()
  , keyboardUpEventSignal :: SignalGen SDLEngine (Signal [Key])
  , keyboardUpEventSink :: Key -> IO ()
  , keyboardPressEventSignal :: SignalGen SDLEngine (Signal [Key])
  , keyboardPressEventSink :: Key -> IO ()

  , windowResizeEventSignal :: SignalGen SDLEngine (Signal [V2 Int])
  , windowResizeEventSink :: V2 Int -> IO ()
  }
