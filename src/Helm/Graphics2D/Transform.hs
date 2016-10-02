-- | Contains the 2D transform matrix type and functions for composing
-- transformation matricies.
module Helm.Graphics2D.Transform
  (
    -- * Types
    Transform(..)
    -- * Composing
  , identity
  , matrix
  , rotation
  , translation
  , scale
  , multiply
  ) where

import qualified Linear.Matrix as Matrix
import           Linear.V2 (V2(V2))
import           Linear.V3 (V3(V3))

-- | Represents a transformation matrix that can be used to transform
-- forms. This is more useful than just using the composing methods
-- in the graphics API, as it can perform skewing and other
-- complex transformation techniques.
data Transform = Transform (Matrix.M33 Double) deriving (Show, Eq, Ord, Read)

instance Num Transform where
  (*) (Transform a) (Transform b) = Transform $ a * b
  (+) (Transform a) (Transform b) = Transform $ a + b
  (-) (Transform a) (Transform b) = Transform $ a - b

  negate (Transform a) = Transform $ negate a
  abs (Transform a) = Transform $ abs a
  signum (Transform a) = Transform $ signum a
  fromInteger n = Transform $ V3 (V3 (fromInteger n) 0 0) (V3 (fromInteger n) 0 0) (V3 0 0 1)

-- | Construct an identity transformation matrix. Anything transformed by this matrix
-- will remain the same.
identity :: Transform
identity = Transform Matrix.identity

-- | Construct a transformation matrix with specific row/column values.
--
-- > matrix a b c d x y
--
-- / a b x \
-- \ c d y /
matrix :: Double -> Double -> Double -> Double -> Double -> Double -> Transform
matrix a b c d x y = Transform $ V3 (V3 a b x) (V3 c d y) (V3 0 0 1)

-- | Construct a counter-clockwise rotating transformation matrix.
--
-- > rotation t
--
-- / cos t  -sin t  0 \
-- \ sin t   cos t  0 /
rotation :: Double -> Transform
rotation t = Transform $ V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1)
  where
    s = sin t
    c = cos t

-- | Construct a translating transformation matrix.
--
-- > translation (V2 x y)
--
-- / 1 0 x \
-- \ 0 1 y /
translation :: V2 Double -> Transform
translation (V2 x y) = Transform $ V3 (V3 1 0 x) (V3 0 1 y) (V3 0 0 1)

-- | Construction a scaling transformation matrix. To scale in all directions,
-- simply have the x and y values the same. Alternatively,
-- to scale by only one direction, keep the excluded dimenion's
-- vector component as 1.
--
-- > scale (V2 x y)
--
-- / x 0 0 \
-- \ 0 y 0 /
scale :: V2 Double -> Transform
scale (V2 x y) = Transform $ V3 (V3 x 0 0) (V3 y 0 0) (V3 0 0 1)

-- | Multiply two transformatio nmatrices together..
multiply :: Transform -> Transform -> Transform
multiply (Transform a) (Transform b) = Transform $ a * b
