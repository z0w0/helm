-- See http://helm-engine.org/guide/animations
-- WIP
import Control.Applicative
import FRP.Elerea.Simple
import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as Text
import qualified FRP.Helm.Automaton as Automaton

animation :: SignalGen (Signal (Double, Double))
animation = Automaton.run (Automaton.stateful (0, 0) (\dt (x, y) -> (x + dt, y + dt))) (0, 0) Time.delta

render :: (Int, Int) -> Time -> (Double, Double) -> Element
render (w, h) dt (dx, dy) = collage w h [filled red $ rect (realToFrac w) (realToFrac h),
                                         move (0, 0) $ toForm $ Text.asText $ round (1 / Time.inSeconds dt)]

main :: IO ()
main = run $ do 
	dimensions <- Window.dimensions
	delta <- Time.delta --Time.delay $ Time.fps 60
	anim <- animation

	return $ render <$> dimensions <*> delta <*> anim
