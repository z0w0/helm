module Main where

import           Test.Framework (defaultMain, testGroup)
import qualified Time

main :: IO ()
main = defaultMain [testGroup "Time" Time.tests]
