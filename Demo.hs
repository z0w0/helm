import Control.Applicative
import FRP.Elerea.Simple
import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data State = State { mx :: Double, my :: Double }

step :: (Int, Int) -> State -> State
step (dx, dy) state = state { mx = (realToFrac dx) + mx state, my = (realToFrac dy) + my state }

render :: (Int, Int) -> State -> Element
render (w, h) (State { .. }) = collage w h [move (mx, my) $ filled (rgb 1 1 1) $ square 100]

main :: IO ()
main = run $ do
  dims <- Window.dimensions
  arrows <- Keyboard.arrows
  stepper <- transfer (State { mx = 0, my = 100 }) step arrows

  return $ render <$> dims <*> stepper
