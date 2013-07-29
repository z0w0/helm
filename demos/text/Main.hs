{-| WIP.
    See http://helm-engine.org/guide/text/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Text as Text

render :: (Int, Int) -> Element
render (w, h) = centeredCollage w h [toForm $ Text.text $ Text.color red $ Text.header $ Text.italic $ Text.toText "wat m8"]

main :: IO ()
main = run config $ render <~ Window.dimensions
  where
    config = defaultConfig { windowTitle = "Helm - Text" }
