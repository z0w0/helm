# Helm

## Introduction

Helm is a functionally reactive game engine written in Haskell and built around
the [Elerea](https://github.com/cobbpg/elerea) FRP framework. Helm is
heavily inspired by the [Elm programming language](http://elm-lang.org) (especially the API).
All rendering is done through a vector-graphics based API. At the core, Helm is
built on SDL and the Cairo vector graphics library. This may change to a more
robust setup in the future, such as a lightweight homebrewed renderer built on OpenGL.
But for now, Cairo performs pretty well.

In Helm, every piece of input that can be gathered from a user (or the operating system)
is hidden behind a signal. For those unfamiliar with FRP, signals are essentially
a value that changes over time. This sort of architecture used for a game allows for pretty
simplistic (and in my opinion, artistic) code.

## Example

The following examples is the barebones of a game. It shows how to create
an accumulated state that depends on the values sampled from signals (e.g. mouse input and such).
You should see a white square on the screen and pressing the arrow keys allows you to move it.

```haskell
import Control.Applicative
import FRP.Elerea.Simple
import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data State = State { mx :: Double, my :: Double }

step :: (Int, Int) -> State -> State
step (dx, dy) state = state { mx = (realToFrac dx) + mx state, my = (realToFrac dy) + my state }

render :: (Int, Int) -> State -> Element
render (w, h) (State { .. }) = collage w h [move (mx, my) $ filled white $ square 100]

main :: IO ()
main = run $ do
  dims <- Window.dimensions
  arrows <- Keyboard.arrows
  stepper <- transfer (State { mx = 0, my = 100 }) step arrows

  return $ render <$> dims <*> stepper
```

## Installing and Building

Helm requires GHC 7.6.4 (Elerea doesn't work with older versions due to a compiler bug).
To install the latest (stable) version from the Hackage repository, use:

```
cabal install helm
```

Alternatively to get the latest development version, you can clone this repository and then run:

```
cabal install
```

You may need to jump a few hoops to install the Cairo bindings (which are a dependency),
which unfortunately is out of my hands.

## License

Helm is licensed under the MIT license. See the `LICENSE` file for more details.
