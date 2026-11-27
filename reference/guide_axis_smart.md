# Smart axis labels for ggplot2

`guide_axis_smart()` is a drop-in replacement for
[`ggplot2::guide_axis()`](https://rdrr.io/pkg/ggplot2/man/guide_axis.html).
It is meant for discrete axes that carry more category labels than the
panel can display at once. Instead of letting the labels overlap, it

## Usage

``` r
guide_axis_smart(
  title = ggplot2::waiver(),
  which = NULL,
  n = NULL,
  nrow = NULL,
  padding = 0.25,
  gap = 0.3,
  extend = 0,
  connection = TRUE,
  connection_style = c("diagonal", "straight", "elbow"),
  link_length = 3,
  connection_col = NULL,
  connection_lwd = 0.5,
  connection_lty = 1,
  tick = c("all", "shown", "none"),
  theta_side = c("outside", "inside"),
  angle = NULL,
  panel_width = NULL,
  max_nrow = 8L,
  hjust = NULL,
  vjust = NULL,
  minor.ticks = FALSE,
  cap = "none",
  order = 0,
  position = ggplot2::waiver(),
  theme = NULL
)
```

## Arguments

- title:

  A character string or
  [`waiver()`](https://rdrr.io/pkg/ggplot2/man/waiver.html) for the axis
  title.

- which:

  Which labels should be displayed? `NULL` (the default) displays all
  labels that fit. Otherwise one of

  - a numeric vector of positions (indices into the vector of breaks),

  - a character vector of label values,

  - a logical vector of the same length as the breaks,

  - a function of two arguments, `function(labels, positions)`, that
    returns a logical vector (or a numeric vector of indices). A
    zero-length vector (`integer(0)`, `character(0)`, `logical(0)`)
    selects no labels at all and draws an empty axis.

  Facets are supported: the guide is built once per panel (or once per
  column for a shared axis), so a character `which` such as
  `c("gene1", "gene2")` only marks the gene in the panel where it
  actually appears. Unmatched labels are silently skipped, which keeps a
  per-panel request quiet.

- n:

  Maximum number of labels to display. When `which` is `NULL` and `n` is
  set, `n` evenly spaced labels are picked. When `which` is given, at
  most `n` of the selected labels are kept.

- nrow:

  Number of parallel tracks used for the labels: rows for a horizontal
  axis, columns for a vertical axis. `NULL` (the default) picks the
  number of tracks automatically: labels stay in one track whenever they
  fit, and are distributed over several tracks only when they crowd into
  a small region. When a track count is supplied explicitly and a label
  does not fit, the label is dropped (with a message) instead of being
  drawn on top of another one.

- padding:

  Minimal gap between two label boxes in a track, as a fraction of the
  label height. Defaults to `0.25`. On a radial (`theta`) axis it is the
  gap between neighbouring labels measured along the arc.

- gap:

  Gap between two neighbouring tracks, as a fraction of the label
  height. Defaults to `0.30`.

- extend:

  How far a label may be pushed beyond the axis range, as a fraction of
  the axis range. The default `0` keeps every label inside the panel, so
  labels are never clipped at the edge of the device. A positive value
  gives the layout more freedom but lets labels overhang the panel,
  which requires enough `plot.margin`.

- connection:

  Draw leader lines between ticks and labels? Defaults to `TRUE`.

- connection_style:

  Shape of the leader line. `"diagonal"` (the default, the style used by
  `circlize` and
  [`ComplexHeatmap::anno_mark()`](https://rdrr.io/pkg/ComplexHeatmap/man/anno_mark.html))
  is a short segment perpendicular to the axis, then a diagonal, then
  another short perpendicular segment ending at the label. `"straight"`
  is a single line from the tick to the label and `"elbow"` is a
  perpendicular segment followed by a shelf parallel to the axis.

- link_length:

  Length in millimetres of the leader line between the axis and the
  first track of labels. Defaults to `3`.

- connection_col, connection_lwd, connection_lty:

  Appearance of the leader lines. `connection_col = NULL` uses the
  colour of the axis ticks.

- tick:

  Which ticks should be drawn: `"all"` (default), `"shown"` (only the
  ticks of the displayed labels) or `"none"`.

- theta_side:

  Place the radial labels `"outside"` (the default) or `"inside"` the
  circle. Inside labels sit on a label track inside the circle and grow
  towards the centre, which suits circular heatmaps with a large
  `inner.radius`. Ignored on cartesian axes.

- angle:

  Rotation of the labels in degrees, `NULL` to inherit the theme.

- panel_width:

  Width (for an x axis) or height (for a y axis) of the panel in
  centimetres. Only used to estimate the number of tracks before the
  plot is rendered; `NULL` (the default) estimates it from the graphics
  device. Set it when the automatic estimate produces too few tracks.

- max_nrow:

  Upper bound for the automatic number of tracks. Defaults to `8`.

- hjust, vjust:

  Horizontal/vertical justification of a label inside its box. `NULL`
  (the default) centres the text in the box, which keeps the leader
  lines aligned with the visual centre of the label.

- minor.ticks:

  **\[experimental\]** Draw minor ticks, passed on to
  [`ggplot2::guide_axis()`](https://rdrr.io/pkg/ggplot2/man/guide_axis.html).

- cap, order, position, theme:

  Passed on to
  [`ggplot2::guide_axis()`](https://rdrr.io/pkg/ggplot2/man/guide_axis.html).

## Value

A `guide` object that can be used in
[`guides()`](https://rdrr.io/pkg/ggplot2/man/guides.html) or in the
`guide` argument of a scale.

## Details

1.  displays only the labels selected with `which` (and/or at most `n`
    evenly spaced labels),

2.  distributes the displayed labels over `nrow` parallel tracks (rows
    for an x axis, columns for a y axis),

3.  connects every displayed label to its tick with a leader line, and

4.  reserves exactly as much axis space as the label stack needs, so the
    available page space is used efficiently.

The one-dimensional arrangement of the labels inside each track is a
port of the `smartAlign` algorithm from the
[circlize](https://github.com/jokergoo/circlize) package.

[`coord_radial()`](https://rdrr.io/pkg/ggplot2/man/coord_radial.html)
renames the position scales to `theta` and `r`, and only accepts guides
that know about the `theta` aesthetic; `guide_axis_smart()` is one of
them. On a radial axis the labels are laid out the way
[`circlize::circos.labels()`](https://rdrr.io/pkg/circlize/man/circos.labels.html)
does it: the text is rotated along the radius so that it points towards
the centre of the circle (turned around on the left half, so nothing is
upside down) and every label is tied to its tick with a three-segment
leader line – a short radial stub, a diagonal, then a short radial stub
at the label.

`which` / `n` pick the categories that appear around the ring,
`tick = "shown"` only keeps their ticks, and `theta_side` chooses
between a label track `"outside"` the circle (the default) and one
attached to the `inner.radius()` of the circle (`"inside"`, useful for
the hole of a circular heatmap). Labels that crowd into one stretch of
the circle are spread along the arc with the `smartAlign` algorithm
(`connection = FALSE` removes the leader lines), and a `start`/`end` arc
(a half circle) is supported as well. Every label stays anchored on its
own tick and a crowded group is spread symmetrically around that group,
so the first label keeps sitting next to its tick instead of the whole
group sliding towards the end of the arc.

The label size, colour, font family, face and line height always come
from the theme's axis text element (`axis.text.x` / `axis.text.y`),
exactly like the stock axis labels; `angle`, `hjust` and `vjust`
override the theme only when they are supplied.

## See also

[`smart_align()`](https://junjunlab.github.io/ggutils/reference/smart_align.md),
[`smart_stack()`](https://junjunlab.github.io/ggutils/reference/smart_stack.md),
[`ggplot2::guide_axis()`](https://rdrr.io/pkg/ggplot2/man/guide_axis.html),
[`ggplot2::guide_axis_theta()`](https://rdrr.io/pkg/ggplot2/man/guide_axis_theta.html)

## Examples

``` r
library(ggplot2)

d <- data.frame(
  sample = factor(paste0("S", 1:40)),
  value  = rnorm(40)
)

# 40 categories: labels are spread over several rows and connected to
# their ticks with leader lines
ggplot(d, aes(sample, value)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis_smart())


# only a hand-picked subset of labels is displayed
ggplot(d, aes(sample, value)) +
  geom_point() +
  scale_x_discrete(
    guide = guide_axis_smart(which = c("S1", "S10", "S20", "S30", "S40"))
  )


# on a vertical axis the labels are stacked in columns
ggplot(d, aes(value, sample)) +
  geom_point() +
  scale_y_discrete(guide = guide_axis_smart(which = c("S1", "S20", "S40")))


# `coord_radial()` (ggplot2 >= 3.5) names the position scales `theta` and `r`;
# attach the guide with `guides(theta = ...)` and it draws the selected labels
# along the ring, like `guide_axis_theta()`
ggplot(d, aes(sample, value)) +
  geom_col() +
  coord_radial(theta = "x") +
  guides(theta = guide_axis_smart(which = seq(1, 40, by = 4)))


# the categories can also be the `theta` scale when they are mapped to y
ggplot(d, aes(value, sample)) +
  geom_col() +
  coord_radial(theta = "y") +
  guides(theta = guide_axis_smart(n = 10))


# a crowded run of labels is spread along the arc and connected to its ticks
# with the three-segment leader lines
ggplot(d, aes(sample, value)) +
  geom_col() +
  coord_radial(theta = "x") +
  guides(theta = guide_axis_smart(which = 9:20))


# only part of the circle (a half circle) works the same
ggplot(d, aes(sample, value)) +
  geom_col() +
  coord_radial(theta = "x", start = -0.5 * pi, end = 0.5 * pi) +
  guides(theta = guide_axis_smart(which = 9:20))


# heat maps below use per-sample z-scores: each sample standardised across
# the 300 genes of the bundled matrix
z_sample <- as.data.frame(as.table(scale(
  tapply(airway_heatmap$df$z,
         list(airway_heatmap$df$gene, airway_heatmap$df$sample), mean)
)))
names(z_sample) <- c("gene", "sample", "z_sample")
z_sample <- merge(z_sample, airway_heatmap$genes[c("gene", "gene_set")],
                  by = "gene")

# a circular heatmap from the bundled RNA-seq data (300 genes x 8 samples):
# the genes run around the circle, labels outside
data(airway_heatmap)
ggplot(z_sample, aes(gene, sample, fill = z_sample)) +
  geom_tile() +
  coord_radial(theta = "x", inner.radius = 0.2) +
  guides(theta = guide_axis_smart(which = airway_heatmap$mark)) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, name = "z-score (sample)")


# the same heatmap with the labels attached to the inner radius (inside the
# hole); a large inner radius leaves room for them
ggplot(z_sample, aes(gene, sample, fill = z_sample)) +
  geom_tile() +
  coord_radial(theta = "x", inner.radius = 0.62) +
  guides(theta = guide_axis_smart(
    which = airway_heatmap$mark, theta_side = "inside"
  )) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, name = "z-score (sample)")


# with facets every panel is a circle of its own
d_facet <- data.frame(
  panel = rep(c("A", "B"), each = 20),
  cat   = factor(rep(paste0("g", 1:20), 2), levels = paste0("g", 1:20)),
  value = rnorm(40)
)
ggplot(d_facet, aes(cat, value)) +
  geom_col(fill = "#3b82f6") +
  coord_radial(theta = "x") +
  guides(theta = guide_axis_smart(which = seq(1, 20, by = 2))) +
  facet_wrap(~panel)


# a faceted circular heatmap needs scales = "free_x": each panel then has its
# own gene scale, so the ring is complete and only that panel's genes are
# labelled
ggplot(z_sample, aes(gene, sample, fill = z_sample)) +
  geom_tile() +
  coord_radial(theta = "x", inner.radius = 0.3) +
  guides(theta = guide_axis_smart(which = unlist(airway_heatmap$mark_by_set))) +
  facet_wrap(~gene_set, ncol = 2, scales = "free_x") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, name = "z-score (sample)")

```
