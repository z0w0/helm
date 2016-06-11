{-| Contains the command type and related utilities. -}
module Helm.Cmd (
  -- * Types
  Cmd(..),
  -- * Utilities
  batch,
  none,
  execute
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)

import Helm.Engine (Engine)

data Cmd a = Cmd (StateT Engine IO [a])

batch :: [Cmd a] -> Cmd a
batch cmds = Cmd $ do
  lists <- sequence $ map (\(Cmd monad) -> monad) cmds

  return $ concat lists

none :: Cmd a
none = Cmd $ return []

execute :: IO a -> (a -> b) -> Cmd b
execute monad f = Cmd $ do
  result <- f <$> lift monad

  return [result]
