---
layout: guide
title: Gradients - Helm, a functionally reactive game engine
section: Gradients
permalink: /guide/gradients/
---

## Gradients

### Color Stops

Helm's drawing API supports both linear and radial gradients. Independent of
a radial or linear gradient is the concept of *color stops*, which are a tuple pair
of a percentage describing at what point the color should be placed along the path
taken by the point and, of course, the color itself. For example, the following
list of color stops would result in the starting point of the gradient being
black and then transitioning to white by the end of the gradient:

{% highlight haskell %}
stopsA :: [(Double, Color)]
stopsA = [(0, black), (1, white)]
{% endhighlight %}

And for reference, a list of stops that evenly transitioned from black to red to yellow
would like:

{% highlight haskell %}
stopsB :: [(Double, Color)]
stopsB = [(0, black), (0.5, red), (1, yellow)]
{% endhighlight %}

### Creating Gradients

According to the [FRP.Helm.Color documentation](http://hackage.haskell.org/packages/archive/helm/latest/doc/html/FRP-Helm-Color.html),
the two functions for creating gradients have the following type signatures:

{% highlight haskell %}
linear :: (Double, Double) -> (Double, Double) -> [(Double, Color)] -> Gradient
radial :: (Double, Double) -> Double -> (Double, Double) -> Double -> [(Double, Color)] -> Gradient
{% endhighlight %}

The *linear* signature is the more obvious of the two. It takes a starting position (in the form of a tuple),
an ending position and a list of color stops, creating a new gradient value. The gradient created
will essentially compute the line segment from the starting position to the ending position. Then, for
each of the percentage values in the color stops, the gradient will transition to the color stop's
color at that percentage along this line segment. Using this knowledge let's try declaring a linear
gradient using the *linear* function and the second list of color stops we previously defined, *stopsB*.

{% highlight haskell %}
linearGrad :: Gradient
linearGrad = linear (0, 0) (0, 100) stopsB
{% endhighlight %}

According to this definition, *linearGrad* is a linear gradient that transitions from black to
red at *(0, 50)* and then finally yellow at *(0, 100)*. It's important to note that gradients
need to be defined in pixel coordinates relative to the shape that it is applied to. For example,
a if *linearGrad* was used with a rectangular shape with height *100*, the black part would start
at the top side and the yellow part would finish at exactly the bottom side.

On the other hand, the *radial* function takes a starting position, a starting (or initial) radius,
an ending position, an ending radius and a list of color stops. Radial gradients start at an initial
position with an initial radius, essentially transitioning towards the end position and radius
in a radial shape. The following is an example declaration of a radial gradient using the previously
defined *stopsA*:

{% highlight haskell %}
radialGrad :: Gradient
radialGrad = radial (0, 0) 0 (0, 0) 64 stopsA
{% endhighlight %}

This creates a radial gradient that starts at *(0, 0)* with an initial radius of *0*. It doesn't
translate towards an end position because it's the same as the starting position, instead
growing to a circle of *64* radius.

### Creating Forms from Gradients and Shapes

[FRP.Helm.Graphics](http://hackage.haskell.org/packages/archive/helm/latest/doc/html/FRP-Helm-Graphics.html)
provides a function to create a form from a gradient and shape, in a similar manner to the previously
discussed *filled* function.

{% highlight haskell %}
gradient :: Gradient -> Shape -> Form
{% endhighlight %}

### Final Product

The final product renders a *300x100* rectangle at *(100, 100)* filled with the *linearGrad* gradient,
and a circle with a radius of *64* at *(500, 300)* filled with the *radialGrad* gradient.

[The code is published as a demo on Github](https://github.com/z0w0/helm/blob/master/demo/gradients.hs).

![final](/img/guide/gradients.png)
