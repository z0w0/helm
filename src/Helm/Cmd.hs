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

-- | Combined a list of mapped commands into a single one.
batch ::
  Engine e
  => [Cmd e a]  -- ^ The list of mapped commands.
  -> Cmd e a    -- ^ The mapped commands accumulated.
batch cmds = Cmd $ do
  lists <- mapM (\(Cmd m) -> m) cmds

  return $ concat lists

-- | A mapped command that does nothing.
none :: Engine e => Cmd e a
none = Cmd $ return []

-- | Execute an IO monad and then map it to a game action.
-- This can be used as a kind of 'liftIO'.
execute ::
  Engine e
  => IO b      -- ^ The IO monad to execute.
  -> (b -> a)  -- ^ The function to map the monad result to an action.
  -> Cmd e a   -- ^ The mapped command.
execute monad f = Cmd $ do
  result <- f <$> lift monad

  return [result]
