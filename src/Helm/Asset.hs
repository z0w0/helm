-- | Contains the file asset types.
module Helm.Asset
  (
    -- * Type Families
    Image
  ) where

-- | Represents an image asset loaded by an engine instance.
--
-- This is a type family, where the instance
-- types are the specific internal representations of an image
-- for an engine. Hence the e type variable here should refer
-- to an 'Engine' instance, but that is not strictly required.
--
-- Having the image type be a family allows us to separate the internal
-- representation of the image assets for each engine from
-- the core Helm library.
data family Image e
