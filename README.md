<p align="center">
  <a href="http://helm-engine.org" title="Homepage"><img src="http://helm-engine.org/img/logo-alt.png" /></a>
  <br>
  <br>
  <a href="https://circleci.com/gh/z0w0/helm" title="CircleCI"><img src="https://circleci.com/gh/z0w0/helm.svg?style=svg" /></a>
</p>

Helm is **currently looking for co-maintainers**. If you would like to help to develop Helm further, please contact me.

## Introduction

Helm is a purely functional game engine written in Haskell and built with
the [Elerea functionally-reactive programming framework](https://github.com/cobbpg/elerea)
and [SDL2](https://www.libsdl.org/). Helm was originally inspired by the
[Elm programming language](http://elm-lang.org).

In Helm, every piece of input that can be gathered from a user (or the operating system)
is contained in a subscription, which is essentially 
as a collection of input events changing over time mapped to game interactions.

Think of it this way - when you hold down the w and a keys, two keyboard events are being captured at every moment.
You might want your game to move your character forward by pressing `w`.
When you add a subscription to your game, you choose how to map these two input events
into a game action type (which you provide, the engine doesn't have any concept
of how the action works). So if you mapped the `w` key to some game action variant (game
actions are usually represented as a collection of data type variants), and the `w` key was held down,
then at every game tick the game would produce a `w` key press event and turn this into
the relevant game action.

On top of subscriptions, Helm has another core concept called commands.
Commands are essentially IO-like monads that have context about the engine state.
Like subscriptions, commands are mapped directly to game actions. This
means that when interacting with IO through Helm, you directly
specify how the result maps to a game action and allows you to make logical
conclusions about how certain monadic results should interact with your game.

Helm provides a structure familiar to MVC-based framework developers.
There is a model (which represents the state of your game), 
a view of the current model (i.e. what's actually shown on the screen) and a function similiar
to a controller that folds the model forward based off of input actions (which are in turn
mapped to from subscription events).

This presents a powerful paradigm shift for game development. Instead of writing event listeners,
Helm treats input events as first-class citizens of the type system, and the actual interaction
between the game state and input events becomes immediately clearer.

## Features

* Interactions between input and game logic is made clear by events and game actions being treated
  first-class by the engine
* Color composition via `Helm.Color`
* 2D vector graphics rendering via `Helm.Graphics2D`
  * Advanced text rendering via `Helm.Graphics2D.Text`
  * Matrix-based 2D transformations (for advanced techniques like skewing) via `Helm.Graphics2D.Transform`
* Keyboard event interactions via `Helm.Keyboard`
* Mouse event interactions via `Helm.Mouse`
* Command-related utilities such as batching via `Helm.Cmd`
* Subscription-related utilities, such as batching and lifting IO-likes via `Helm.Sub`
* Time-based event interactions via `Helm.Time`
* Window event interactions and other utilities via `Helm.Window`
* The base functionality of Helm is separate from the backend engine implementation, so
  custom media frameworks (which generally handle rendering, input, etc.) can be integrated
  with Helm quite easily. At the moment, the only available implementation is SDL2
  (which is currently bundled with the game engine) however the plan is to have more options in the future.

## Installing and Building

Before you can use Helm, you'll to follow the
[Gtk2Hs installation guide](https://wiki.haskell.org/Gtk2Hs/Installation)
(which is required for the Haskell Cairo bindings). Additionally, Helm
requires a GHC version of 7.6 or higher.

Using [Stack](https://haskellstack.org) when working with Helm is recommended.
To install Helm with Stack, use:

```
stack install helm
```

It's best to add Helm as a dependency in your game's Cabal file rather
than installing it globally, however if you're new to the engine, installing
it globally will let you run the example Helm games. See the next section.

## Getting Started

Check out the `examples` directory for some examples; the `hello` example is a
particularly good start and `flappy` is a bit more advanced. We could always
use more examples so if you end up making something cool and lightweight that
you'd think would be a good one, feel free to open a pull request!

If you have installed Helm globally using Stack, you can run the `flappy` example using:

```
stack exec helm-example-flappy
```

Or the `hello` example using:

```
stack exec helm-example-hello
```

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
   
