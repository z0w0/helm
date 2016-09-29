-- | Contains the SDL engine types.
module Helm.Engine.SDL.Engine
  (
    -- * Types
    SDLEngine(..)
  , SDLEngineConfig(..)
  ) where

import           Control.Monad (when)
import           Data.Word (Word32)

import qualified Data.Text as T
import           FRP.Elerea.Param (Signal, SignalGen, externalMulti)
import           Linear.Affine (Point(P))
import           Linear.Metric (distance)
import           Linear.V2 (V2(V2))
import qualified SDL
import           SDL (Keysym(..))
import qualified SDL.Event as Event
import qualified SDL.Init as Init
import qualified SDL.Time as Time
import qualified SDL.Video as Video
import           SDL.Video (WindowConfig(..))
import qualified SDL.Video.Renderer as Renderer

import           Helm.Engine (Engine(..), MouseButton, Key)
import           Helm.Graphics (Graphics(..))
import           Helm.Graphics2D (Collage)

-- | A data structure describing how to run the SDL engine.
-- Use 'defaultConfig' and then only change the data fields that you need to.
data SDLEngineConfig = SDLEngineConfig
  { windowDimensions :: V2 Int
  , windowIsFullscreen :: !Bool
  , windowIsResizable :: !Bool
  , windowTitle :: !String
  }

-- | A data structure describing the SDL engine's state.
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
