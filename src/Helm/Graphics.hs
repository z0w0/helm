-- | Contains the graphics type.
module Helm.Graphics (
  -- * Types
  Graphics(..)
) where

import Helm.Graphics2D (Collage)

-- The graphics type contains any form of structure that
-- produces visual graphics to the screen, i.e. either 2D or 3D elements.
data Graphics i = Graphics2D (Collage i)
