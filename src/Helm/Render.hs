module Helm.Render (
  -- * Types
  Render(..),
  -- * Utilities
  none
) where

import Control.Monad.Trans.State (StateT)

import Helm.Engine (Engine)

data Render a = Render (StateT Engine IO a)

{-| Render nothing to the screen. -}
none :: Render ()
none = Render $ return ()
