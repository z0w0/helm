module Helm.Graphics2D.Transform (
  -- * Types
  Transform(..),
  -- * Composing
  identity,
  matrix,
  rotation,
  translation,
  scale,
  multiply
) where

import Linear.V3 (V3(V3))
import qualified Linear.Matrix as Matrix

data Transform = Transform (Matrix.M33 Double) deriving (Show, Eq, Ord, Read)

instance Num Transform where
  (*) (Transform a) (Transform b) = Transform $ a * b
  (+) (Transform a) (Transform b) = Transform $ a + b
  (-) (Transform a) (Transform b) = Transform $ a - b

  negate (Transform a) = Transform $ negate a
  abs (Transform a) = Transform $ abs a
  signum (Transform a) = Transform $ signum a
  fromInteger n = Transform $ V3 (V3 (fromInteger n) 0 0) (V3 (fromInteger n) 0 0) (V3 0 0 1)

identity :: Transform
identity = Transform Matrix.identity

matrix :: Double -> Double -> Double -> Double -> Double -> Double -> Transform
matrix a b c d x y = Transform $ V3 (V3 a b x) (V3 c d y) (V3 0 0 1)

rotation :: Double -> Transform
rotation t = Transform $ V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1)
  where
    s = sin t
    c = cos t

translation :: Double -> Double -> Transform
translation x y = Transform $ V3 (V3 1 0 x) (V3 0 1 y) (V3 0 0 1)

scale :: Double -> Double -> Transform
scale x y = Transform $ V3 (V3 x 0 0) (V3 y 0 0) (V3 0 0 1)

multiply :: Transform -> Transform -> Transform
multiply (Transform a) (Transform b) = Transform $ a * b
