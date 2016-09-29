-- | Contains the graphics type.
module Helm.Graphics
  (
    -- * Types
    Graphics(..)
  ) where

import Helm.Graphics2D (Collage)

-- The graphics type contains any form of structure that
-- produces visual graphics to the screen, i.e. either 2D or 3D elements.
--
-- The type variable e should refer to an 'Engine' instance, however
-- is not strictly required to refer to an instance.
data Graphics e = Graphics2D (Collage e)
