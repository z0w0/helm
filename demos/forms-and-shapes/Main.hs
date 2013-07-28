{-| WIP.
    See http://helm-engine.org/guide/gradients/ -}
module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window

render :: (Int, Int) -> Element
render (w, h) = centeredCollage w h [rotate 0.5 $ filled red $ square 64]

main :: IO ()
main = run $ fmap (fmap render) Window.dimensions
