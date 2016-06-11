{-| Contains signals that sample input from the game window. -}
module Helm.Window (
  -- * Commands
  size,
  width,
  height,
  -- * Subscriptions
  resizes
) where

import Control.Monad.State (get)
import Helm.Cmd (Cmd(..))
import Helm.Engine (Engine(..))
import Helm.Sub (Sub(..))
import Linear.V2 (V2(V2))

import qualified SDL
import qualified SDL.Video as Video

size :: Cmd (V2 Int)
size = Cmd $ do
  Engine { window } <- get
  V2 x y <- SDL.get $ Video.windowSize window

  return [V2 (fromIntegral x) (fromIntegral y)]

width :: Cmd Int
width = Cmd $ do
  Engine { window } <- get
  V2 x _ <- SDL.get $ Video.windowSize window

  return [fromIntegral x]

height :: Cmd Int
height = Cmd $ do
  Engine { window } <- get
  V2 _ y <- SDL.get $ Video.windowSize window

  return [fromIntegral y]

resizes :: (V2 Int -> a) -> Sub a
resizes _ = Sub $ return $ return []
