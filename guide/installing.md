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

* Cairo 1.12
* SDL 1.2

You will also need to install GHC 7.6 and it's accompanying Cabal. Once installed, use
the following to install the Helm haskell package:

{% highlight bash %}
cabal update
cabal install gtk2hs-buildtools
cabal install helm
{% endhighlight %}

The gtk2hs dependency needs to be installed manually because it is required to build
the Cairo bindings. You can now start using Helm to make games!

### Windows

Coming soon!

### Mac OSX

Coming soon!
