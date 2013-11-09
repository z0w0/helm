{-| Contains signals that sample input from the mouse. -}
module FRP.Helm.Mouse (
  -- * Types
  Mouse(..),
  -- * Position
  isDown,
  -- * Mouse State
  position, x, y
) where

import Control.Applicative
import FRP.Elerea.Simple
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as Util

{-| A data structure describing a button on a mouse. -}
data Mouse
  = LeftMouse
  | MiddleMouse
  | RightMouse
  | X1Mouse
  | X2Mouse deriving (Show, Eq, Ord, Read)

{- All integer values of this enum are equivalent to the SDL key enum. -}
instance Enum Mouse where
  fromEnum LeftMouse = 1
  fromEnum MiddleMouse = 2
  fromEnum RightMouse = 3
  fromEnum X1Mouse = 4
  fromEnum X2Mouse = 5

  toEnum 1 = LeftMouse
  toEnum 2 = MiddleMouse
  toEnum 3 = RightMouse
  toEnum 4 = X1Mouse
  toEnum 5 = X2Mouse
  toEnum _ = error "FRP.Helm.Mouse.Mouse.toEnum: bad argument"

{-| The current position of the mouse. -}
position :: SignalGen (Signal (Int, Int))
position = effectful $ (\(x_, y_, _) -> (x_, y_)) <$> SDL.getMouseState

{-| The current x-coordinate of the mouse. -}
x :: SignalGen (Signal Int)
x = effectful $ (\(x_, _, _) -> x_) <$> SDL.getMouseState

{-| The current y-coordinate of the mouse. -}
y :: SignalGen (Signal Int)
y = effectful $ (\(_, y_, _) -> y_) <$> SDL.getMouseState

{-| The current state of a certain mouse button.
    True if the mouse is down, false otherwise. -}
isDown :: Mouse -> SignalGen (Signal Bool)
isDown m = effectful $ (\(_, _, b_) -> elem (toEnum $ fromIntegral $ fromEnum m) b_) <$> SDL.getMouseState
