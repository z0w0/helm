module FRP.Helm.Engine where
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Data.Map as Map
{-| A data structure describing the current engine state. -}
data Engine = Engine {
  window   :: SDL.Window,
  renderer :: SDL.Renderer,
  cache    :: Map.Map FilePath Cairo.Surface,
  continue :: Bool
}
