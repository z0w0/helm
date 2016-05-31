{-| Contains signals that sample input from the mouse. -}
module FRP.Helm.Backend.SDL.Mouse
(
  -- * Position
  position, x, y,
  -- * Mouse State
  isDown,
  isDownButton,
  clicks
) where

import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Backend.SDL.Engine
import FRP.Helm.Sample
import FRP.Helm.Signal
import SDL.Input.Mouse
import Linear.V2 (V2(V2))
import Linear.Affine (Point(P))

{-| The current position of the mouse. -}
position :: Signal Engine (Int, Int)
position = Signal $ getPosition >>= transfer (pure (0,0)) update
  where
    getPosition = effectful $ do
      P (V2 x_ y_) <- getAbsoluteMouseLocation
      return (fromIntegral x_, fromIntegral y_)

{-| The current x-coordinate of the mouse. -}
x :: Signal Engine Int
x = fst <~ position

{-| The current y-coordinate of the mouse. -}
y :: Signal Engine Int
y = snd <~ position

{-| The current state of the left mouse-button. True when the button is down,
    and false otherwise. -}
isDown :: Signal Engine Bool
isDown = isDownButton ButtonLeft

{-| The current state of a given mouse button. True if down, false otherwise.
    -}
isDownButton :: MouseButton -> Signal Engine Bool
isDownButton btn = Signal $ getDown >>= transfer (pure False) update
  where
    getDown = effectful $ do
      btnMap <- getMouseButtons
      return (btnMap btn)

{-| Always equal to unit. Event triggers on every mouse click. -}
clicks :: Signal Engine ()
clicks = Signal $ signalGen isDown >>= transfer (pure ()) update_
  where update_ _ (Changed True) _ = Changed ()
        update_ _ _ _              = Unchanged ()
