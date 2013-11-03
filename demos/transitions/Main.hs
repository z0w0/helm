{-| WIP.
    See http://helm-engine.org/guide/transitions/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Transition as Transition

render :: Color -> (Int, Int) -> Element
render bg (w, h) = centeredCollage w h [filled bg $ rect (fromIntegral w) (fromIntegral h)]

main :: IO ()
main = run config $ render <~ Transition.transition delta status transition ~~ Window.dimensions
  where
    config = defaultConfig { windowTitle = "Helm - Text" }
    transition = Transition.fromList [(white, 0), (green, 2), (red, 5), (black, 1), (yellow, 2)]
    delta = constant $ Time.fps 60
    status = constant Transition.Cycle
