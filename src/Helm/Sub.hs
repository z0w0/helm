-- | Contains the subscription type and related utilities.
module Helm.Sub
  (
    -- * Types
    Sub(..)
    -- * Utilities
  , batch
  , none
  ) where

import Helm.Engine (Engine, Sub(..))

-- | Combine a list of subscriptions into a single one.
-- This is allows for subscriptions to multiple input events to be
-- combined into one subscription that encompasses all the actions
-- mapped from events.
batch
  :: Engine e
  => [Sub e a]  -- ^ The list of subscriptions.
  -> Sub e a    -- ^ The subscriptions accumulated.
batch subs = Sub $ do
  signals <- mapM (\(Sub gen) -> gen) subs

  return $ do
    lists <- sequence signals

    return $ concat lists

-- | A subscription that does nothing. If user input events
-- aren't required for a game, return this in the subscriptions function
-- of the game.
none :: Engine e => Sub e a
none = Sub . return $ return []
