module Main where

import Test.Framework (defaultMain, testGroup)
import qualified Color
import qualified Mouse
import qualified Keyboard
import qualified Time

main :: IO ()
main = defaultMain [testGroup "Color" Color.tests,
                    testGroup "Keyboard" Keyboard.tests,
                    testGroup "Mouse" Mouse.tests,
                    testGroup "Time" Time.tests]
