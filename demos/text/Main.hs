{-| WIP.
    See http://helm-engine.org/guide/text/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Text as Text

render :: Time -> (Int, Int) -> Element
render _ (w, h) = centeredCollage w h [toForm $ Text.text $ Text.color red $ Text.header $ Text.italic $ Text.toText "wat m8"]

main :: IO ()
main = run config $ render <~ Time.delay (Time.fps 60) ~~ Window.dimensions
  where
    config = defaultConfig { windowTitle = "Helm - Text" }
