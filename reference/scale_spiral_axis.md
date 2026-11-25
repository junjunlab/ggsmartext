# Fine-tune the axes drawn along a spiral

`scale_x_spiral_axis()` and `scale_y_spiral_axis()` are ordinary
continuous scales that additionally carry the settings of the axis which
[`coord_spiral()`](https://junjunlab.github.io/ggutils/reference/coord_spiral.md)
paints along the spiral: where the axis sits, how long its ticks are and
how its labels are turned.

## Usage

``` r
scale_x_spiral_axis(
  ...,
  discrete = FALSE,
  axis_at = NULL,
  tick_length = 0.007,
  label_rot = NULL,
  label_size = NULL,
  show = TRUE
)

scale_y_spiral_axis(
  ...,
  side = c("end", "start", "both"),
  axis_at = NULL,
  tick_length = 0.008,
  label_rot = NULL,
  label_size = NULL,
  max_labels = NULL,
  show = TRUE
)
```

## Arguments

- ...:

  Passed on to
  [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
  /
  [`ggplot2::scale_x_discrete()`](https://ggplot2.tidyverse.org/reference/scale_discrete.html)
  or
  [`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
  for example `breaks`, `minor_breaks`, `labels`, `limits` or `trans`.
  `minor_breaks` draws the short unlabelled ticks between two breaks;
  the coord paints them at 55% of `tick_length`, and
  `minor_breaks = NULL` leaves them out.

- discrete:

  Use a discrete scale instead of a continuous one, for a factor or
  character x aesthetic (one violin or box per category). Only
  `scale_x_spiral_axis()` needs it; `breaks` then takes the category
  values.

- axis_at:

  Put the radial (y) ruler at a fixed direction instead, in degrees
  measured counter-clockwise from the east (right) direction: `0` is
  right, `90` top, `180` left. The last crossing of that direction on
  the outermost turn is used. `NULL` keeps the `side` behaviour. Ignored
  by `scale_x_spiral_axis()`, whose axis runs at every break along the
  spiral.

- tick_length:

  Length of the ticks, in npc units (the panel is 1 x 1). Use `0` to
  hide the ticks. The coord shortens the ticks (and keeps the labels
  just outside them) so that both stay inside the white gap between two
  neighbouring turns, whatever `height` / `gap` is.

- label_rot:

  Rotation of the labels in degrees. `NULL` (the default) turns every
  label along the spiral, so it follows the arc; a number turns all
  labels by that fixed angle, e.g. `90` for always-vertical labels.

- label_size:

  Text size in points. `NULL` inherits `axis.text.x` / `axis.text.y`
  from the theme.

- show:

  Draw this axis at all? `FALSE` hides the ticks and labels while
  keeping the data, which is handy for facets where the axis is
  redundant.

- side:

  Where the radial (y) ruler is drawn: at the `"start"` of the spiral,
  at its `"end"` (the default) or on `"both"` sides, exactly like the
  `side` argument of `spiralize::spiral_yaxis()`. Every tick is a short
  segment that runs along the spiral, and its label is turned to the
  tangent at that point, so it reads along the arc and never ends up
  upside down. Only used by `scale_y_spiral_axis()`; ignored when
  `axis_at` is given.

- max_labels:

  Maximum number of radial labels kept when the track is too thin for
  all the breaks. `NULL` keeps every break. Only used by
  `scale_y_spiral_axis()`.

## Value

A ggplot2 scale that also carries the spiral axis settings.

## Details

Use them exactly like
[`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
/
[`ggplot2::scale_y_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html);
`breaks`, `minor_breaks`, `labels`, `limits`, `trans`, ... keep their
usual meaning, so the ticks shown on the spiral are the breaks of the
scale.

Both axes size themselves against the white gap between two turns: the
ticks are shortened to fit, the labels sit just outside them, and the
number of radial labels follows the thickness of the track and the
height of the device. Nothing has to be tuned by hand, but every piece
can still be overridden through the arguments below.

## Examples

``` r
library(ggplot2)
d <- data.frame(x = seq(0, 1, length.out = 200),
                y = sin(seq(0, 1, length.out = 200) * 6 * pi) * .35 + .5)

ggplot(d, aes(x, y)) +
  geom_line() +
  scale_x_spiral_axis(breaks = seq(0, 1, by = .1), label_size = 6) +
  scale_y_spiral_axis(breaks = c(.3, .5, .7), axis_at = 0, label_rot = 90) +
  coord_spiral()
#> Error in scale_x_spiral_axis(breaks = seq(0, 1, by = 0.1), label_size = 6): could not find function "scale_x_spiral_axis"
```
