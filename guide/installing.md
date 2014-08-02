---
layout: guide
title: Installing - Helm, a functionally reactive game engine
section: Installing
permalink: /guide/installing/
---

## Installing

### Linux

Helm is mainly developed and tested on Linux, so it is obviously the easiest
platform to develop with. To install Helm on Linux, you will first need to
install all the dependencies through your package manager. Helm depends
on the the following development libraries being present:

* Cairo 1.x.x
* SDL 2.0.x
* Pango 1.x.x

You will also need to install GHC 7.6 and it's accompanying Cabal. Confirm that
the installed GHC is definitely version 7.6 (or later) by checking that the output of
`ghc --version` is something similar to the following:

{% highlight bash %}
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.6.3
{% endhighlight %}

Once GHC has been installed, use the following to install the Helm haskell package:

{% highlight bash %}
$ cabal update # if this is your first time using cabal
$ cabal install gtk2hs-buildtools
$ cabal install helm
{% endhighlight %}

The gtk2hs dependency needs to be installed manually because it is required to build
the Cairo bindings. You can now start using Helm to make games!

### Mac OS X

You will first need to install the [Haskell Platform 2013.2.0.0 for Mac](http://www.haskell.org/platform/mac.html)
and the Xcode command line tools. The complete instructions for this process are on the linked download page. Once
the Haskell platform has been installed through its installer, confirm that the installed version of GHC
is definitely 7.6 (or later) by checking that the output of `ghc --version` is something similar to the following
in a new Terminal window:

{% highlight bash %}
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.6.3
{% endhighlight %}

In order to install Helm, you will first need to install the same external dependencies discussed
in the Linux section using an OS X package manager. For this guide we'll only be explaining the process
using the Homebrew package manager, but it should be similar tactics for others. Firstly, install
the [latest version of Homebrew from its website](http://brew.sh). You can then run the following
commands to install the dependencies using Homebrew:

{% highlight bash %}
$ brew doctor # if this is your first time using homebrew
$ brew install sdl2
$ brew install cairo --without-x11
$ brew install pango --without-x11
{% endhighlight %}

We need to add the `--without-x11` flag when installing Cairo and Pango because we can't use the X11 backend
on OS X without an X11 server (not that we want to, anyway). We can now run the Cabal package manager
included in the previously installed Haskell platform to install the Helm libraries (in a similar
manner to Linux):

{% highlight bash %}
$ cabal update # if this is your first time using cabal
$ cabal install gtk2hs-buildtools
$ cabal install helm
{% endhighlight %}

You're ready to start making games!

### Windows

Coming soon!
