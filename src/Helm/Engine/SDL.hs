{-# LANGUAGE TypeFamilies #-}
-- | Contains the SDL engine implementation of Helm.
module Helm.Engine.SDL
  (
    -- * Types
    SDLEngine
  , SDLEngineConfig(..)
    -- * Startup
  , defaultConfig
  , startup
  , startupWith
    -- * Asset Loading
  , withImage
  ) where

import Helm.Engine.SDL.Engine
 ( SDLEngine
 , SDLEngineConfig(..)
 , defaultConfig
 , startup
 , startupWith
 , withImage
 )

