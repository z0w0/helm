-- | Contains the command type and related utilities.
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

-- | Combine a list of commands into a single one.
batch
  :: Engine e
  => [Cmd e a]  -- ^ The list of commands to combine.
  -> Cmd e a    -- ^ The accumulated command.
batch cmds = Cmd $ do
  lists <- mapM (\(Cmd m) -> m) cmds

  return $ concat lists

-- | A command that does nothing. When returned in a Helm
-- game's update or initial functions, it will not produce
-- any game actions.
none :: Engine e => Cmd e a
none = Cmd $ return []

-- | Execute an IO monad and then map its result to a game action.
-- This can be used as a kind of 'liftIO', however to keep
-- things consistent with the rest of the library, you
-- must map the monad result a game action.
execute
  :: Engine e
  => IO b      -- ^ The IO monad to execute.
  -> (b -> a)  -- ^ The function to map the monad result to an action.
  -> Cmd e a   -- ^ The mapped command.
execute monad f = Cmd $ do
  result <- f <$> lift monad

  return [result]
