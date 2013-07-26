-- See http://helm-engine.org/guide/gradients/
import FRP.Helm
import qualified FRP.Helm.Window as Window

stopsA :: [(Double, Color)]
stopsA = [(0, black), (1, white)]

stopsB :: [(Double, Color)]
stopsB = [(0, black), (0.5, red), (1, yellow)]

linearGrad :: Gradient
linearGrad = linear (0, 0) (0, 100) stopsB

radialGrad :: Gradient
radialGrad = radial (0, 0) 0 (0, 0) 64 stopsA

render :: (Int, Int) -> Element
render (w, h) = collage w h [move (-100, -100) $ gradient linearGrad $ rect 300 100,
                             move (100, 100) $ gradient radialGrad $ circle 64]

main :: IO ()
main = run $ fmap (fmap render) Window.dimensions
