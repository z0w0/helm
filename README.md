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

## Features

* Allows you to express game logic dependent on input in a straightforward manner,
  treating events as (almost) first class objects (the essence of FRP).
* Vector graphics based rendering, allow you to either write art
  designed for any resolution or still load generic images and render
  those as you would with any pixel-based direct blitting game engine.
* Straightforward API heavily inspired by the Elm programming language.

## Future Work

* Improve the API. There's a few API calls from Elm that would work
  just as nicely in Helm. These are marked inside TODOs in the code.
  There also other important things that it's missing,
  such as audio, joysticks and loading a larger range of
  image formats.
* Backend wise, it would be nice to use OpenGL instead of Cairo.
  Cairo isn't particuarly that well performing for graphic intensive games,
  although work is done being towards to fix that. However, using
  OpenGL would make the engine more lightweight, easier to port
  and be incredibly easier to accelerate. This means I have
  to write the full vector graphics stack myself, but the worse part
  will probably just be line styles, the rest should be moderately easy.
  This will also allow loading of multiple image formats, as the current
  reason for not using SDL_image is that it's annoying as fuck
  to integrate with Cairo. Helm also currently uses the Cairo toy text
  API for rendering, which isn't suppose to be used in production. If switched
  to OpenGL, SDL_ttf would be a better fit.
* Optimizations and testing. This is the first release of the engine so
  obviously little testing or optimizations have been done.
  It's a little hard to set up a test framework for a game engine,
  but I have a few ideas, such as writing a dummy version of the backend
  that simply renders to a PNG file that is fed fake (but predictable) input,
  which is then compared to a static PNG file to see if the final expected
  rendering outcome was achieved.
* Port and support multiple platforms. I've only been testing it on
  Linux, but there's really no reason that it wouldn't work out of the box
  on Windows or OSX after setting up the dependencies. But I'd definitely
  also like to investigate Android and iOS.

## Example

The following examples is the barebones of a game. It shows how to create
an accumulated state that depends on the values sampled from signals (e.g. mouse input and such).
You should see a white square on the screen and pressing the arrow keys allows you to move it.

```haskell
{-# LANGUAGE RecordWildCards #-}

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

Helm requires GHC 7.6 (Elerea doesn't work with older versions due to a compiler bug).
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
