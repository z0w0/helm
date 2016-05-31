{-# LANGUAGE ScopedTypeVariables #-}
{-| Contains miscellaneous utility functions and the main
    functions for interfacing with the engine. -}
module FRP.Helm (
  -- * Types
  Time,
  -- * Engine
  run,
  defaultConfig,
  -- * Prelude
  module Utilities,
  module Signal,
  FRP.Helm.Signal.lift
) where


import Control.Exception
import Control.Monad (when)

import FRP.Elerea.Param hiding (Signal)
import FRP.Helm.Utilities as Utilities
import FRP.Helm.Sample
import FRP.Helm.Signal as Signal hiding (lift)
import qualified FRP.Helm.Signal (lift)
import FRP.Helm.Time (Time)

import FRP.Helm.Backend

{-| Initializes and runs the game engine. The supplied signal generator is
    constantly sampled for an element to render until the user quits.

    > import FRP.Helm
    > import qualified FRP.Helm.Window as Window
    >
    > render :: (Int, Int) -> Element
    > render (w, h) = collage w h [rect (fromIntegral w) (fromIntegral h) |> filled red]
    >
    > main :: IO ()
    > main = run defaultConfig $ lift render Window.dimensions
 -}
run :: BEngine engine => BConfig engine -> Signal engine (BElement engine) -> IO ()
run config element = do
  engine <- startup config
  let (Signal gen) = initApplication <~ element
                                     ~~ dimensions
                                     ~~ continue'
                                     ~~ exposedSignal
  (start gen >>= run' engine) `finally` engineFinalizer engine

continue' :: BEngine engine => Signal engine Bool
continue' = (==0) <~ count quitSignal

{-| A utility function called by 'run' that samples the element
    or quits the entire engine if events say to do so. -}
run' :: BEngine engine => engine -> (engine -> IO (Sample (BApplication engine))) -> IO ()
run' engine smp = when (continueExecution engine) $ smp engine >>= renderIfChanged engine
                                                               >>= flip run' smp
