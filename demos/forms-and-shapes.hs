-- See http://helm-engine.org/guide/forms-and-shapes
-- WIP
import FRP.Helm
import qualified FRP.Helm.Window as Window

render :: (Int, Int) -> Element
render (w, h) = collage w h [move (400, 300) $ rotate 0.5 $ filled red $ square 64]

main :: IO ()
main = run $ fmap (fmap render) Window.dimensions
