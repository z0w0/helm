<p align="center">
  <a href="http://helm-engine.org" title="Homepage"><img src="http://helm-engine.org/img/logo-alt.png"/></a>
</p>

## Introduction

Helm is a functionally reactive game engine written in Haskell and built around
the [Elerea FRP framework](https://github.com/cobbpg/elerea). Helm is
heavily inspired by the [Elm programming language](http://elm-lang.org) (especially the API).
All rendering is done through a vector-graphics based API. At the core, Helm is
built on SDL and the Cairo vector graphics library.

In Helm, every piece of input that can be gathered from a user (or the operating system)
is hidden behind a signal. For those unfamiliar with FRP, signals are essentially
a value that changes over time. This sort of architecture used for a game allows for pretty
simplistic (and in my opinion, artistic) code.

Documentation of the Helm API is available on [Hackage](http://hackage.haskell.org/package/helm).
There is currently a heavily work-in-progress guide on [Helm's website](http://helm-engine.org/guide),
which is a resource aiming to give thorough explanations of the way Helm and its API work through examples.

## Features

* Allows you to express game logic dependent on input in a straightforward manner,
  treating events as first class values (the essence of FRP).
* Vector graphics based rendering, allow you to either write art
  designed for any resolution or still load generic images and render
  those as you would with any pixel-blitting engine.
* Straightforward API heavily inspired by the Elm programming language. The API
  is broken up into the following areas:
  * `FRP.Helm` contains the main code for interfacing with the game engine but
    also includes some utility functions and the modules `FRP.Helm.Color` and `FRP.Helm.Graphics`
    in the style of a sort of prelude library, allowing it to be included and readily
    make the most basic of games.
  * `FRP.Helm.Automaton` contains the `Automaton` data structure and functions
    for composing, creating and calculating them. Automatons are a useful
    abstraction of a dynamic process that is fed input from a signal
    and feeds output through a signal. This is really useful for things
    like animation systems, accumulating network packets and other
    stateful but input dependent things.
  * `FRP.Helm.Color` contains the `Color` data structure, functions for composing
    colors and a few pre-defined colors that are usually used in games.
  * `FRP.Helm.Graphics` contains all the graphics data structures, functions
    for composing these structures and other general graphical utilities.
  * `FRP.Helm.Joystick` contains signals for working with joystick state.
  * `FRP.Helm.Keyboard` contains signals for working with keyboard state.
  * `FRP.Helm.Mouse` contains signals for working with mouse state.
  * `FRP.Helm.Text` contains functions for composing text, formatting it
    and then turning it into an element.
  * `FRP.Helm.Time` contains functions for composing units of time and signals that sample from the game clock.
  * `FRP.Helm.Window` contains signals for working with the game window state.

## Example

The simplest example of a Helm game that doesn't require any input from the user is the following:

```haskell
import FRP.Helm
import qualified FRP.Helm.Window as Window

render :: (Int, Int) -> Element
render (w, h) = collage w h [move (100, 100) $ filled red $ square 64]

main :: IO ()
main = run $ fmap (fmap render) Window.dimensions
```

It renders a red square at the position `(100, 100)` with a side length of 64px.  
  
The next example is the barebones of a game that depends on input. It shows how to create
an accumulated state that depends on the values sampled from signals (e.g. mouse input).
You should see a white square on the screen and pressing the arrow keys allows you to move it.

```haskell
import Control.Applicative
import FRP.Elerea.Simple
import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data State = State { mx :: Double, my :: Double }

step :: (Int, Int) -> State -> State
step (dx, dy) state = state { mx = (realToFrac dx) + mx state,
                              my = (realToFrac dy) + my state }

render :: (Int, Int) -> State -> Element
render (w, h) (State { mx = mx, my = my }) =
  centeredCollage w h [move (mx, my) $ filled white $ square 100]

main :: IO ()
main = run $ do
    dimensions <- Window.dimensions
    arrows <- Keyboard.arrows
    stepper <- transfer state step arrows

    return $ render <$> dimensions <*> stepper

  where
    state = (State { mx = 0, my = 0 })
```

Checkout the demos folder for more examples.

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

Helm is licensed under the MIT license. See the LICENSE file for more details.

## Contributing

Helm would benefit from either of the following contributions:

1. Try out the engine, reporting any issues or suggestions you have.
2. Look through the source, get a feel for the code and then
   contribute some features or fixes. If you plan on contributing
   code please submit a pull request and follow the formatting
   styles set out in the current code: 2 space indents, documentation
   on every top-level function, favouring monad operators over
   do blocks when there is a logical flow of data, spaces between operators
   and after commas, etc. Please also confirm that the code passes under
   HLint.

The following is a list of major issues that need to be tackled in the future:

* Improve the API. See [issue #4](https://github.com/z0w0/helm/issues/4).
* Backend wise, it would be nice to use GLFW/OpenGL instead of SDL/Cairo (at the very least SDL/OpenGL).
  See [issue #1](https://github.com/z0w0/helm/issues/1).
* Optimizations and testing. This is a early release of the engine so
  obviously little testing or optimizations have been done.
  See [issue #2](https://github.com/z0w0/helm/issues/2).
* Port and support multiple platforms. I've only been testing it on
  Linux, but there's really no reason that it wouldn't work out of the box
  on Windows or OSX after setting up the dependencies.
  See [issue #3](https://github.com/z0w0/helm/issues/3).
