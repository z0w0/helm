{-# LANGUAGE TypeFamilies #-}
{-| Defines the interface a backend has to provide in order to
    be used by Helm. -}
module FRP.Helm.Backend (
-- * Types
  BEngine(..)
) where

import Control.Monad.Trans.State

import FRP.Helm.Sample
import FRP.Helm.Signal

--type BSignalGen engine a = Elerea.SignalGen engine (Elerea.Signal (Sample a))

class BEngine engine where
  type RenderMonad engine :: * -> *

  type Helm engine a
  type Helm engine a = StateT engine (RenderMonad engine) a
  data BApplication engine :: *
  data EngineConfig engine :: *
  type BElement engine :: *

  defaultConfig :: EngineConfig engine

  initApplication :: BElement engine -> (Int, Int) -> Bool -> () -> BApplication engine
  startup :: EngineConfig engine -> IO engine
  engineFinalizer :: engine -> IO ()

  exposedSignal :: Signal engine ()
  quitSignal :: Signal engine ()
  continueExecution :: engine -> Bool

  renderIfChanged :: engine -> Sample (BApplication engine) -> IO engine

  dimensions :: Signal engine (Int, Int)
  position :: Signal engine (Int, Int)

  width :: Signal engine Int
  width = fst <~ dimensions

  height :: Signal engine Int
  height = snd <~ dimensions
