{-| Contains the subscription type and related utilities. -}
module Helm.Sub (
  -- * Types
  Sub(..),
  -- * Utilities
  batch,
  none
) where

import FRP.Elerea.Param (SignalGen, Signal)

import Helm.Engine (Engine)

data Sub a = Sub (SignalGen Engine (Signal [a]))

batch :: [Sub a] -> Sub a
batch subs = Sub $ do
  signals <- sequence $ map (\(Sub gen) -> gen) subs

  return $ do
    lists <- sequence signals

    return $ concat lists

none :: Sub a
none = Sub $ return $ return []
