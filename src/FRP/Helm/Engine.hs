module FRP.Helm.Engine where
import qualified SDL.Video as Video
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Data.Map as Map
{-| A data structure describing the current engine state. -}
data Engine = Engine {
  window   :: Video.Window,
  renderer :: Video.Renderer,
  cache    :: Map.Map FilePath Cairo.Surface,
  continue :: Bool
}
