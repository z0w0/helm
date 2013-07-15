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

According to the [FRP.Helm.Color documentation](/todo), the two functions for creating
gradients have the following type signatures:

{% highlight haskell %}
linear :: (Double, Double) -> (Double, Double) -> [(Double, Color)] -> Gradient
radial :: (Double, Double) -> Double -> (Double, Double) -> Double -> [(Double, Color)] -> Gradient
{% endhighlight %}

The *linear* signature is the more obvious of the two. It takes a starting position (in the form of a tuple),
an ending position and a list of color stops, creating a new gradient value. The gradient created
will essentially compute the line segment from the starting position to the ending position. Then, for
each of the percentage values in the color stops, the gradient will transition to the color stop's
color at that percentage along this line segment.

On the other hand, the *radial* function takes a starting position, a starting (or initial) radius,
an ending position, an ending radius and a list of color stops.
