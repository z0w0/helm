import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Engine.SDL as SDL

data Action = Idle | ChangePosition (Double, Double)
data Model = Model (Double, Double)

initial :: (Model, Cmd SDLEngine Action)
initial = (Model (0, 0), Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update model (ChangePosition pos) = (Model pos, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Mouse.moves (\(V2 x y) -> ChangePosition (fromIntegral x, fromIntegral y))

view :: Model -> Graphics
view (Model pos) = Graphics2D $ collage 800 600 [move pos $ filled (rgb 1 0 0) $ rect 10 10]

main = do
  engine <- SDL.startup

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
