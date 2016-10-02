-- | Contains the graphics type.
module Helm.Graphics
  (
    -- * Types
    Graphics(..)
  ) where

import Helm.Graphics2D (Collage)

-- | Represents any form of structure that produces visual
-- graphics to the screen, e.g. 2D or 3D graphics.
--
-- The type variable e should refer to an 'Engine' instance.
data Graphics e = Graphics2D (Collage e)
