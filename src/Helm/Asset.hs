{-| Contains the types for loading game assets, e.g. images, audio, etc. -}
module Helm.Asset (
  -- * Types
  Image(..),
  Sound(..)
) where

newtype Image = Image ()
newtype Sound = Sound ()
