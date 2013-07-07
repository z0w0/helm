module FRP.Helm.Mouse (position, x, y, isDown, Mouse(..)) where

import Control.Applicative
import FRP.Elerea.Simple
import qualified Graphics.UI.SDL as SDL

data Mouse = LeftMouse | MiddleMouse | RightMouse

-- |The current mouse position.
position :: SignalGen (Signal (Int, Int))
position = effectful $ (\(x_, y_, _) -> (x_, y_)) <$> SDL.getMouseState

-- |The current x-coordinate of the mouse.
x :: SignalGen (Signal Int)
x = effectful $ (\(x_, _, _) -> x_) <$> SDL.getMouseState

-- |The current y-coordinate of the mouse.
y :: SignalGen (Signal Int)
y = effectful $ (\(_, y_, _) -> y_) <$> SDL.getMouseState

mapMouse m =
  case m of
    LeftMouse -> SDL.ButtonLeft
    MiddleMouse -> SDL.ButtonMiddle
    RightMouse -> SDL.ButtonRight

{-| The current state of a certain mouse button.
    True if the mouse is down, false otherwise. -}
isDown :: Mouse -> SignalGen (Signal Bool)
isDown m = effectful $ (\(_, _, b_) -> elem (mapMouse m) b_) <$> SDL.getMouseState

{-
isClicked :: SignalGen (Signal Bool)
 -}
