{-| Contains signals that sample input from joysticks. -}
module FRP.Helm.Joystick (
  -- * Types
  Joystick,
  -- * Probing
  available,
  name,
  open,
  index,
  availableAxes,
  availableBalls,
  availableHats,
  availableButtons,
  -- * Joystick State
  axis,
  hat,
  button,
  ball
) where

import Control.Applicative
import Data.Int (Int16)
import FRP.Elerea.Simple
import qualified Graphics.UI.SDL as SDL

{-| A type describing a joystick. -}
type Joystick = SDL.Joystick

{-| The amount of joysticks available. -}
available :: SignalGen (Signal Int)
available = effectful SDL.countAvailable

{-| The name of a joystick. Can throw an exception when sampled if the joystick index is invalid. -}
name :: Int -> SignalGen (Signal String)
name i = effectful $ SDL.name i

{-| The joystick at a certain slot. Can throw an exception when sampled if the joystick index is invalid. -}
open :: Int -> SignalGen (Signal Joystick)
open i = effectful $ SDL.open i

{-| The index of a joystick. -}
index :: Joystick -> SignalGen (Signal Int)
index j = return $ return $ SDL.index j

{-| The amount of axes available for a joystick. -}
availableAxes :: Joystick -> SignalGen (Signal Int)
availableAxes j = return $ return $ SDL.axesAvailable j

{-| The amount of balls available for a joystick. -}
availableBalls :: Joystick -> SignalGen (Signal Int)
availableBalls j = return $ return $ SDL.ballsAvailable j

{-| The amount of hats available for a joystick. -}
availableHats :: Joystick -> SignalGen (Signal Int)
availableHats j = return $ return $ SDL.hatsAvailable j

{-| The amount of buttons available for a joystick. -}
availableButtons :: Joystick -> SignalGen (Signal Int)
availableButtons j = return $ return $ SDL.buttonsAvailable j

{-| The current state of the axis of the joystick. -}
axis :: Joystick -> Int -> SignalGen (Signal Int)
axis j i = effectful $ SDL.update >> fromIntegral <$> SDL.getAxis j (fromIntegral i)

{-| The current state of the hat of the joystick, returned
    as a directional tuple. For example, up is /(0, -1)/,
    left /(-1, 0)/, bottom-right is /(1, 1)/, etc. -}
hat :: Joystick -> Int -> SignalGen (Signal (Int, Int))
hat j i = effectful $ SDL.update >> hat' <$> SDL.getHat j (fromIntegral i)

{-| A utility function for mapping a list of hat states to an averaged directional tuple. -}
hat' :: [SDL.Hat] -> (Int, Int)
hat' hats = if l > 0 then (round $ fromIntegral hx / l, round $ fromIntegral hy / l) else (0, 0)
  where
    l = realToFrac $ length hats :: Double
    (hx, hy) = foldl hat'' (0, 0) hats

{-| A utility function for accumulating the total directional tuple. -}
hat'' :: (Int, Int) -> SDL.Hat -> (Int, Int)
hat'' (x, y) h =
  case h of
    SDL.HatCentered -> (x, y)
    SDL.HatUp -> (x, y - 1)
    SDL.HatRight -> (x + 1, y)
    SDL.HatDown -> (x, y + 1)
    SDL.HatLeft -> (x - 1, y)
    SDL.HatRightUp -> (x + 1, y - 1)
    SDL.HatRightDown -> (x + 1, y + 1)
    SDL.HatLeftUp -> (x - 1, x - 1)
    SDL.HatLeftDown -> (x - 1, y + 1)

{-| The current state of the button of the joystick. -}
button :: Joystick -> Int -> SignalGen (Signal Bool)
button j i = effectful $ SDL.update >> SDL.getButton j (fromIntegral i)

{-| The current state of the ball of the joystick. -}
ball :: Joystick -> Int -> SignalGen (Signal (Int, Int))
ball j i = effectful $ SDL.update >> ball' <$> SDL.getBall j (fromIntegral i)

{-| A utility function for mapping the optional value to a null tuple or the actual tuple. -}
ball' :: Maybe (Int16, Int16) -> (Int, Int)
ball' mayhaps =
  case mayhaps of
    Just (x, y) -> (fromIntegral x, fromIntegral y)
    Nothing -> (0, 0)
