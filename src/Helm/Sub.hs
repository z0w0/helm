{-| Contains the subscription related utilities. -}
module Helm.Sub
  (
    -- * Types
    Sub(..)
    -- * Utilities
  , batch
  , none
  ) where

import Helm.Engine (Engine, Sub(..))

-- | Combine a list of mapped subscriptions into a single one.
-- This is allows for subscriptions to multiple input events to be
-- combined into one mapped subscription that encompasses all the actions
-- mapped from events.
batch ::
  Engine e
  => [Sub e a]  -- ^ The list of mapped subscriptions.
  -> Sub e a    -- ^ The mapped subscriptions accumulated.
batch subs = Sub $ do
  signals <- mapM (\(Sub gen) -> gen) subs

  return $ do
    lists <- sequence signals

    return $ concat lists

-- | A mapped subscription that does nothing.
none :: Engine e => Sub e a
none = Sub . return $ return []
