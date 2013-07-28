---
layout: guide
title: Colors - Helm, a functionally reactive game engine
section: Colors
permalink: /guide/colors/
---

## Colors

### Creating Colors

Representing colors in Helm is pretty straightforward. All colors
are stored as RGBA values, with each component (R, G, B and A) being
a 64-bit floating point value. This has an advantage over the regular way
of representing RGBA colors (with each component being represented as a single byte)
because it allows a higher range of colors to be composed. Unfortunately this higher range
of colors is still clamped to a 32-bit color when being rendered to the screen, but it is still
incredibly useful for accurately calculating things such as complementary colors, averaging colors, etc.

To create colors you can use the following functions as explained in the
[*FRP.Helm.Color* documentation](http://hackage.haskell.org/packages/archive/helm/latest/doc/html/FRP-Helm-Color.html):

{% highlight haskell %}
rgba :: Double -> Double -> Double -> Double -> Color
rgb :: Double -> Double -> Double -> Color
hsva :: Double -> Double -> Double -> Double -> Color
hsv :: Double -> Double -> Double -> Color
{% endhighlight %}

The *rgb* function just takes three double values (red, green and blue)
that should be in the range *0.0* to *1.0*, where *0.0* is no color and *1.0*
is full color. The *rgba* function is basically the same except it takes an
additional double value for the alpha component (or transparency), where *0.0*
is completely transparent and *1.0* is opaque. In other words, *rgb* creates
a solid color (with an alpha value of *1.0*) and *rgb* can create a variably
transparent color.

For those who prefer HSV (or if you need to do complex color operations in HSV space),
there is also the *hsv* and *hsva* functions. These functions simply convert
from HSV\[A] space to RGB\[A] &mdash; the final representation of the color remains
in RGBA form.

### Using Colors

You don't need to create your own color values for the obviously colors like black, red and blue as these
are predefined in the *FRP.Helm.Graphics* module. A few predefined color constants:

{% highlight haskell %}
red :: Color
lime :: Color
blue :: Color
yellow :: Color
cyan :: Color
magenta :: Color
black :: Color
white :: Color
gray :: Color
grey :: Color -- same as gray for the sanity of international users
maroon :: Color
navy :: Color
green :: Color
teal :: Color
purple :: Color
violet :: Color
forestGreen :: Color
{% endhighlight %}

There might be a few missing from this list, so check out the
[*FRP.Helm.Color* documentation](http://hackage.haskell.org/packages/archive/helm/latest/doc/html/FRP-Helm-Color.html#v:red) just in case.

There are many functions that actually use colors, such as filling or outling shapes.
Check out the [Forms and Shapes section of the guide](/guide/forms-and-shapes) or
the [*FRP.Helm.Graphics* documentation](http://hackage.haskell.org/packages/archive/helm/latest/doc/html/FRP-Helm-Graphics.html#v:filled)
for some examples of these functions.

### Creating a Color Wheel

Let's try making something familiar with our new-found knowledge of colors in Helm &mdash; a color wheel. We want to end up with something
that looks like a pizza made up of differently-colored slices. First let's define a list of colors we actually want to appear in the color wheel:

{% highlight haskell %}
colors :: [Color]
colors = [red, lime, blue, yellow, cyan, magenta, maroon, navy, green, teal, purple]
{% endhighlight %}

Helm doesn't allow assigning specific colors to specific points in a polygon, so we need to create a polygon for each slice in the color wheel.
A good start on a function fingerprint for a function to create one of these slices is something like the following:

{% highlight haskell %}
slice :: Int -> Form
{% endhighlight %}

Where the given integer is the index of the color in the color list. The reason we take the index instead of the color itself is so we
can calculate the position of the points in the slice polygon relative to the other colors (remembering that each color is going to be
assigned its own slice). Let's define the core of this function that we're going to use to build the actual slice generating code:

{% highlight haskell %}
slice :: Int -> Form
slice n = filled color $ polygon points
  where
    color = colors !! n
    r = 150
    points = []
{% endhighlight %}

The *color* variable has been defined as the color in *colors* at index *n* (i.e. the color we want to paint the slice)
and *r* is a constant representing the radius of the color wheel. This won't actually render anything yet, because the
*points* variable is just a blank list. Let's think about how we actually calculate the points for the slice polygon.

Firstly, let's only use 3 points for the slice. This means that the outer edge of the slice will unfortunately be straight, but making it
smooth is over-complicating it for such a simple example. The first point will be at the center of the color wheel (all
slices will have this point in common), the second point will be at some arbitrary point on the circumference of the color-wheel
and the third will be along the circumference at some fixed increment from the second point.

Essentially, in order to construct the slice we need to think of the whole color wheel as containing exactly
*2 \* pi* radians (i.e. a circle). Then we can calculate this fixed increment by dividing all of these radians up
into a certain amount of radians allocated to each slice, based on the number of slices (or number of colors).
Let's model this thought process in actual code:

{% highlight haskell %}
slice :: Int -> Form
slice n = filled color $ polygon points
  where
    color = colors !! n
    r = 150
    increment = 2 * pi / realToFrac (length colors)
    t1 = increment * realToFrac n
    t2 = t1 + increment
    points = [(0, 0)]
{% endhighlight %}

Now we've defined *increment* which, as previously mentioned, is the amount of radians allocated to each slice. The variables
*t1* and *t2* are the angle where the *nth* slice starts and the angle where the slice after it starts, respectively. We have
the angles, now we need to figure out how to find the actual points that represent the outside edge of the slice. To do that
we need to remember the concept of the unit circle and the equations
*x' = r (cos &theta; - sin &theta;)*
and
*y' = r (cos &theta; + sin &theta;)*.

{% highlight haskell %}
pointOnCircum :: Double -> Double -> (Double, Double)
pointOnCircum r theta = (r * (cos theta - sin theta), r * (cos theta + sin theta))
{% endhighlight %}

Using this newly defined function will finally result in what we want: a colored slice on our color wheel.

{% highlight haskell %}
slice :: Int -> Form
slice n = filled color $ polygon points
  where
    color = colors !! n
    increment = 2 * pi / realToFrac (length colors)
    t1 = increment * realToFrac n
    t2 = t1 + increment
    r = 150
    points = [(0, 0), pointOnCircum r t1, pointOnCircum r t2]

{% endhighlight %}

### Final Product

The final product renders a (flat-edged) color wheel with 11 predefined colors on it.

[Checkout the code on Github →](https://github.com/z0w0/helm/blob/master/demos/colors.hs)

![final](/img/guide/colors.png)
