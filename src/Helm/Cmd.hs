{-| Contains the command related utilities. -}
module Helm.Cmd (
  -- * Types
  Cmd(..),
  -- * Utilities
  batch,
  none,
  execute
) where

import Control.Monad.Trans.Class (lift)

import Helm.Engine (Engine, Cmd(..))

batch :: Engine e => [Cmd e a] -> Cmd e a
batch cmds = Cmd $ do
  lists <- mapM (\(Cmd m) -> m) cmds

  return $ concat lists

none :: Engine e => Cmd e a
none = Cmd $ return []

execute :: Engine e => IO a -> (a -> b) -> Cmd e b
execute monad f = Cmd $ do
  result <- f <$> lift monad

  return [result]
