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

batch :: Engine e => [Sub e a] -> Sub e a
batch subs = Sub $ do
  signals <- mapM (\(Sub gen) -> gen) subs

  return $ do
    lists <- sequence signals

    return $ concat lists

none :: Engine e => Sub e a
none = Sub . return $ return []
