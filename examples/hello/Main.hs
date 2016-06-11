import Helm
import Helm.Graphics2D
import Helm.Render.Cairo (render)

import qualified Helm.Cmd as Cmd
import qualified Helm.Sub as Sub

data Action = Idle | ChangeDirection (Double, Double)
data Model = Model (Double, Double)

initial :: (Model, Cmd Action)
initial = (Model (0, 0), Cmd.none)

update :: Model -> Action -> (Model, Cmd Action)
update model _ = (model, Cmd.none)

subscriptions :: Sub Action
subscriptions = Sub.none

view :: Model -> Render ()
view model = render $ collage 800 600 []

main = do
  engine <- startup

  run engine $ GameConfig {
    initialFn       = initial,
    updateFn        = update,
    subscriptionsFn = subscriptions,
    viewFn          = view
  }
