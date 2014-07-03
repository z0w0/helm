module Main where

import Test.Framework (defaultMain, testGroup)
import qualified Color
import qualified Time

main :: IO ()
main = defaultMain [testGroup "Color" Color.tests,
                    testGroup "Time" Time.tests]
