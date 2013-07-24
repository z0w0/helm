module Main where

import Test.Framework (defaultMain, testGroup)
import qualified Color

main :: IO ()
main = defaultMain [testGroup "Color" Color.tests]
