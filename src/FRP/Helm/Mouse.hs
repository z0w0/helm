{-| Contains signals that sample input from the mouse. -}
module FRP.Helm.Mouse
(
  -- * Types
  Mouse(..),
  -- * Position
  position, x, y,
  -- * Mouse State
  isDown,
  isDownButton,
  clicks
) where

import Control.Applicative (pure)
import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Sample
import FRP.Helm.Signal
import qualified Graphics.UI.SDL as SDL

{-| A data structure describing a button on a mouse. -}
data Mouse
  = LeftMouse
  | MiddleMouse
  | RightMouse
  | X1Mouse
  | X2Mouse deriving (Show, Eq, Ord, Read)

{- All integer values of this enum are equivalent to the SDL key enum. -}
instance Enum Mouse where
  fromEnum LeftMouse   = 1
  fromEnum MiddleMouse = 2
  fromEnum RightMouse  = 3
  fromEnum X1Mouse     = 4
  fromEnum X2Mouse     = 5

  toEnum 1 = LeftMouse
  toEnum 2 = MiddleMouse
  toEnum 3 = RightMouse
  toEnum 4 = X1Mouse
  toEnum 5 = X2Mouse
  toEnum _ = error "FRP.Helm.Mouse.Mouse.toEnum: bad argument"

{-| The current position of the mouse. -}
position :: Signal (Int, Int)
position = Signal $ getPosition >>= transfer (pure (0,0)) update
  where
    getPosition = effectful $ alloca $ \xptr -> alloca $ \yptr -> do
      _ <- SDL.getMouseState xptr yptr
      x_ <- peek xptr
      y_ <- peek yptr

      return (fromIntegral x_, fromIntegral y_)

{-| The current x-coordinate of the mouse. -}
x :: Signal Int
x = fst <~ position

{-| The current y-coordinate of the mouse. -}
y :: Signal Int
y = snd <~ position

{-| The current state of the left mouse-button. True when the button is down,
    and false otherwise. -}
isDown :: Signal Bool
isDown = isDownButton LeftMouse

{-| The current state of a given mouse button. True if down, false otherwise.
    -}
isDownButton :: Mouse -> Signal Bool
isDownButton m = Signal $ getDown >>= transfer (pure False) update
  where
    getDown = effectful $ do
      flags <- SDL.getMouseState nullPtr nullPtr

      return $ (.&.) (fromIntegral flags) (fromEnum m) /= 0

{-| Always equal to unit. Event triggers on every mouse click. -}
clicks :: Signal ()
clicks = Signal $ signalGen isDown >>= transfer (pure ()) update_
  where update_ _ (Changed True) _ = Changed ()
        update_ _ _ _              = Unchanged ()

