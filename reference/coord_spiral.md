# Spiral coordinates for ggplot2

`coord_spiral()` bends a normal Cartesian ggplot into an Archimedean
spiral, following the geometry used by the **spiralize** package: the
base spiral is `r = b * theta` with `b` chosen so that one turn is one
radius unit. The x aesthetic runs along the spiral (as the angle) and
the y aesthetic runs across the track (as the radius). Points, lines,
bars and areas are all supported, and facets get one spiral per panel.

## Usage

``` r
coord_spiral(
  start = 90,
  end = 360 * 3 + 90,
  period = NULL,
  clockwise = FALSE,
  inner = 0.04,
  outer = 0.46,
  height = 0.8,
  scale_by = c("angle", "curve"),
  reverse = FALSE,
  flip = c("none", "horizontal", "vertical", "both"),
  labels = TRUE,
  y_axis = TRUE,
  y_axis_theta = NULL,
  y_axis_n = 5L,
  clip = "off",
  xlim = NULL,
  ylim = NULL
)
```

## Arguments

- start, end:

  Angular range of the spiral, **in degrees** (as in
  `spiralize::spiral_initialize()`). `360 * n + a` is a convenient way
  to ask for `n` turns ending at angle `a`.

- period:

  Number of x units per turn. When supplied, `end` is recalculated from
  the x range and `period`.

- clockwise:

  Draw the spiral clockwise? `FALSE` (the default) draws it
  counter-clockwise, like `spiralize()`.

- inner, outer:

  Inner and outer radius of the whole spiral, in panel npc units (0.5 is
  the panel edge). The distance between two turns is
  `(outer - inner) / (end - start) * 360`.

- height:

  Height of the data track as a fraction of the distance between two
  neighbouring turns (0 to 1).

- scale_by:

  `"angle"` maps equal differences in x to equal differences in angle;
  `"curve"` maps them to equal distances along the spiral curve.

- reverse:

  Reverse the y direction inside the track?

- flip:

  One or more of `"none"`, `"horizontal"`, `"vertical"`, `"both"`;
  mirrors the whole spiral.

- labels:

  Draw the x (angle) and y (radial) axis labels along the spiral?

- y_axis:

  Draw the radial (y) axis ruler as well as the angle axis?

- y_axis_theta:

  Angle in degrees at which the radial ruler is placed; `NULL` puts it
  at the outermost end of the spiral.

- y_axis_n:

  Number of ticks on the radial ruler when the y scale has no breaks of
  its own.

- clip:

  Clipping of the panel, passed on to ggplot2.

- xlim, ylim:

  Limits passed on to the coord (usually set on the scales).

## Value

A `CoordSpiral` ggproto object.

## Examples

``` r
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.5.3
d <- data.frame(x = seq(0, 1, length.out = 200),
                y = sin(seq(0, 1, length.out = 200) * 8 * pi))
ggplot(d, aes(x, y)) + geom_line() + coord_spiral()


ggplot(d[1:60, ], aes(x, y)) +
  geom_col() + coord_spiral(end = 360 + 90, height = 0.8)
```
