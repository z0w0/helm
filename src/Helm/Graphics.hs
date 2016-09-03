{-| Contains the graphics type. -}
module Helm.Graphics (
  -- * Types
  Graphics(..)
) where

import Helm.Graphics2D (Element)

data Graphics = Graphics2D Element
