<p align="center">
  <a href="http://helm-engine.org" title="Homepage"><img src="http://helm-engine.org/img/logo-alt.png" /></a>
  <br>
  <br>
  <a href="https://circleci.com/gh/z0w0/helm" title="CircleCI"><img src="https://circleci.com/gh/z0w0/helm.svg?style=svg" /></a>
</p>

## Introduction

Helm is a purely functional game engine written in Haskell and built with
the [Elerea FRP framework](https://github.com/cobbpg/elerea). Helm was
originally inspired by the [Elm programming language](http://elm-lang.org).

In Helm, every piece of input that can be gathered from a user (or the operating system)
is contained in a subscription, which is essentially 
as a collection of input events changing over time. Think of it this way - when you hold down
the w and s keys, two keyboard events are being captured at every moment. In this case, a subscription to keyboard presses
would then yield you with a collection of two events at every game tick.

Helm provides a structure similar to MVC (model-view-controller).
There is a model (which represents the state of your game), 
a view of the current model (i.e. what's actually shown on the screen) and a controller that folds the model
forward based off of input actions (which are mapped from the subscription events).

This presents a powerful paradigm shift for game development. Instead of writing event listeners,
Helm treats input events as first-class citizens of the type system, and the actual interaction between
the game state and input events becomes immediately clearer.

## Features

* Allows you to express game logic dependent on input in a straightforward manner,
  treating events as first class values (the essence of FRP).
* Vector graphics based rendering, allow you to either write art
  designed for any resolution or still load generic images and render
  those as you would with any pixel-blitting engine.
* Straightforward API heavily inspired by the Elm programming language. The API
  is broken up into the following areas:
  * `Helm` contains the main code for interfacing with the game engine but
    also includes some utility functions and the modules `Helm.Color`, `Helm.Utilities`
    and `Helm.Graphics` in the style of a sort of prelude library, allowing it to be included
    and readily make the most basic of games.
  * `Helm.Color` contains the `Color` data structure, functions for composing
    colors and a few pre-defined colors that are usually used in games.
  * `Helm.Graphics` contains all the graphics data structures, functions
    for composing these structures and other general graphical utilities.
  * `Helm.Keyboard` contains signals for working with keyboard state.
  * `Helm.Mouse` contains signals for working with mouse state.
  * `Helm.Random` contains signals for generating random values
  * `Helm.Signal`  constains useful functions for working with signals such
     as lifting/folding
  * `Helm.Text` contains functions for composing text, formatting it
    and then turning it into an element.
  * `Helm.Time` contains functions for composing units of time and time-dependant signals
  * `Helm.Utilities` contains an assortment of useful functions,
  * `Helm.Window` contains signals for working with the game window state.

## Installing and Building

Before you can install Helm, you'll to follow the
[Gtk2Hs installation guide](https://wiki.haskell.org/Gtk2Hs/Installation)
(which is required for the Haskell Cairo bindings). Additionally, Helm
requires a GHC version of 7.6 or higher.

To install the latest stable version from the Hackage repository, use:

```
cabal install helm
```

Alternatively to get the latest development version run:

```
git clone git://github.com/z0w0/helm.git
cd helm
cabal install
```

## Getting Started

Check out the `examples` directory for some examples; the `hello` example is a particularly good start.
Unfortunately, there's little to no example games yet, so if you end up making something cool and lightweight
that you'd think would be a good example, feel free to open a pull request!

## Documentation

API documentation for the latest stable version of Helm is available on [Hackage](http://hackage.haskell.org/package/helm).
Alternatively, if you've cloned this repo, you can build the documentation manually using Haddock.

## License

Helm is licensed under the MIT license. See the LICENSE file for more details.

## Contributing

Helm would benefit from either of the following contributions:

1. Try out the engine, reporting any issues or suggestions you have.
2. Look through the source, get a feel for the code and then
   contribute some features or fixes. If you plan on contributing
   code, please follow
   [Johan Tibell's Haskell style guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
   with the following exceptions:
   * Up to 120 characters per line are allowed (widescreens for life).
   * Use a two space indent.
   * Acronyms in all caps for identifiers (while maintaing camel-case), i.e. SDL or 2D/3D.
   
