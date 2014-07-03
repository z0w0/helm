module Color where

import FRP.Helm.Color
import Test.HUnit hiding (Test)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests = [testCase "gray is grey" (gray @=? grey),
         testCase "hsv sane for black" (black @=? hsv 0 0 0),
         testCase "hsv sane for white" (white @=? hsv 0 0 1),
         testCase "hsv sane for red" (red @=? hsv 0 1 1),
         testCase "hsv sane for lime" (lime @=? hsv 120 1 1),
         testCase "hsv sane for blue" (blue @=? hsv 240 1 1),
         testCase "hsv sane for yellow" (yellow @=? hsv 60 1 1),
         testCase "hsv sane for cyan" (cyan @=? hsv 180 1 1),
         testCase "hsv sane for magenta" (magenta @=? hsv 300 1 1),
         testCase "hsv sane for gray" (gray @=? hsv 0 0 0.5),
         testCase "hsv sane for maroon" (maroon @=? hsv 0 1 0.5),
         testCase "hsv sane for navy" (navy @=? hsv 240 1 0.5),
         testCase "hsv sane for green" (green @=? hsv 120 1 0.5),
         testCase "hsv sane for teal" (teal @=? hsv 180 1 0.5),
         testCase "hsv sane for purple" (purple @=? hsv 300 1 0.5),
         testCase "hsv sane for violet" (violet @=? trunc (hsv 300 0.4496 0.923)),
         testCase "hsv sane for forestGreen" (forestGreen @=? trunc (hsv 120 0.755 0.543))]

-- We only need a few digits to be right to mark them sane. Hard to be accurate beyond 3, really.
trunc :: Color -> Color
trunc (Color r g b a) = rgba (trunc' r 3) (trunc' g 3) (trunc' b 3) a

trunc' :: Double -> Integer -> Double
trunc' f n = fromInteger (round $ f * (10 ^ n)) / (10.0 ^^ n)
