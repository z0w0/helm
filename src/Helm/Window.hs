{-| Contains signals that sample input from the game window. -}
module Helm.Window
  (
    -- * Commands
    size
  , width
  , height
    -- * Subscriptions
  , resizes
  ) where

import Control.Monad.State (get)
import Control.Monad.IO.Class (liftIO)

import FRP.Elerea.Param (input, snapshot)
import Linear.V2 (V2(V2))

import Helm.Engine (Engine(..), Cmd(..), Sub(..))

size :: Engine e => Cmd e (V2 Int)
size = Cmd $ do
  engine <- get
  V2 x y <- liftIO $ windowSize engine

  return [V2 (fromIntegral x) (fromIntegral y)]

width :: Engine e => Cmd e Int
width = Cmd $ do
  engine <- get
  V2 x _ <- liftIO $ windowSize engine

  return [fromIntegral x]

height :: Engine e => Cmd e Int
height = Cmd $ do
  engine <- get
  V2 _ y <- liftIO $ windowSize engine

  return [fromIntegral y]

resizes :: Engine e => (V2 Int -> a) -> Sub e a
resizes f = Sub $ do
  engine <- input >>= snapshot

  fmap (fmap f) <$> windowResizeSignal engine
