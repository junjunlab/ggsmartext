# Cluster axis guide for ggplot2

`guide_axis_cluster()` draws a hierarchical clustering dendrogram along
a discrete x or y axis. The companion scales `scale_x_cluster()` and
`scale_y_cluster()` cluster the categories themselves, so the categories
are re-ordered into dendrogram leaf order and the tree is aligned to the
new positions automatically.

## Usage

``` r
guide_axis_cluster(
  data = NULL,
  method = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median",
    "centroid"),
  height = 1,
  col = "black",
  lwd = 0.7,
  type = c("rectangle", "triangle"),
  hclust = NULL,
  free = NULL,
  map_data = NULL,
  .tree_labels = NULL,
  cluster_position = NULL,
  show_labels = TRUE,
  draw_tree = TRUE,
  title = ggplot2::waiver(),
  position = ggplot2::waiver(),
  order = 0,
  theme = NULL,
  ...
)

scale_x_cluster(
  data = NULL,
  method = "complete",
  height = 1,
  ring_gap = NULL,
  col = "black",
  lwd = 0.7,
  type = c("rectangle", "triangle"),
  free = NULL,
  breaks = NULL,
  labels = ggplot2::waiver(),
  limits = NULL,
  expand = ggplot2::waiver(),
  position = "bottom",
  tree_position = NULL,
  strip_side = c("below", "above"),
  name = ggplot2::waiver(),
  guide = NULL,
  value = NULL,
  .map_data = NULL,
  ...
)

scale_y_cluster(
  data = NULL,
  method = "complete",
  height = 1,
  col = "black",
  lwd = 0.7,
  type = c("rectangle", "triangle"),
  free = NULL,
  breaks = NULL,
  labels = ggplot2::waiver(),
  limits = NULL,
  expand = ggplot2::waiver(),
  position = "left",
  tree_position = NULL,
  strip_side = c("below", "above"),
  name = ggplot2::waiver(),
  guide = NULL,
  value = NULL,
  .map_data = NULL,
  ...
)
```

## Arguments

- data:

  A numeric matrix or data frame whose rows are the categorical levels
  to cluster ([`rownames()`](https://rdrr.io/r/base/colnames.html) give
  the labels) and whose columns are the features used for clustering.
  Alternatively an `hclust` object, or — for `scale_x_cluster()` /
  `scale_y_cluster()` — omitted, in which case the matrix is derived
  from the plot's own data (see Details).

- method:

  Clustering method passed to
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html). One of
  `"ward.D"`, `"ward.D2"`, `"single"`, `"complete"`, `"average"`,
  `"mcquitty"`, `"median"` or `"centroid"`.

- height:

  Overall height of the dendrogram, in centimetres. For a cartesian y
  axis this is the width of the tree. On a
  [`coord_radial()`](https://rdrr.io/pkg/ggplot2/man/coord_radial.html)
  theta axis it is the thickness of the ring dendrogram (inwards for
  `position = "bottom"`, outwards for `"top"`); for an r-axis tree on a
  partial arc it is the angular width of the wedge
  (`min(pi/3, 0.30 * height)` radians).

- col:

  Colour of the dendrogram lines.

- lwd:

  Line width of the dendrogram lines.

- type:

  Shape of the dendrogram. `"rectangle"` (the default) draws rectangular
  branches, like `plot.dendrogram(type = "rectangle")`. `"triangle"`
  draws diagonal branches, like `plot.dendrogram(type = "triangle")`.

- hclust:

  An optional `hclust` object. When supplied, `data` and `method` are
  ignored. This is used internally by `scale_x_cluster()` and
  `scale_y_cluster()` so that the scale and the guide share one
  clustering.

- free:

  For use with `facet_wrap(scales = "free")`. When `TRUE`, the
  dendrogram is computed **per panel** from the subset of `data` rows
  that match that panel's categories, so each facet gets its own tree
  and its own leaf order. When `data` is omitted (see
  `scale_x_cluster()`) the per-panel matrix is rebuilt from the plot
  data instead, so no matrix is required. See the "Faceted free scales"
  section below.

- map_data:

  Internal. Long frame (`.focal`, `.other`, `.value`) used to rebuild
  the matrix of a free-scale panel; filled in automatically by the
  cluster scales.

- .tree_labels:

  Internal. The raw category names, used to match a tree's leaves when
  the displayed labels differ from the axis values (free facets re-key
  them per panel).

- cluster_position:

  Where the tree sits when the guide is attached directly with
  [`guides()`](https://rdrr.io/pkg/ggplot2/man/guides.html); defaults to
  the axis side (`"bottom"` for an x axis, `"left"` for a y axis).

- show_labels:

  When `FALSE` the labels keep their layout but are drawn invisibly, so
  only the tree shows – used when a secondary axis carries the tree
  while the main axis prints the category names.

- draw_tree:

  Internal switch used by `scale_x_cluster()`: `FALSE` draws only the
  labels, and the tree is injected into the finished gtable instead
  (that is what `position = "top"` needs).

- title:

  Axis title, passed on to
  [`ggplot2::guide_axis()`](https://rdrr.io/pkg/ggplot2/man/guide_axis.html).

- position:

  Where the dendrogram is drawn: `"bottom"`/`"top"` for an x axis,
  `"left"`/`"right"` for a y axis. The category names always stay on the
  usual side (bottom for x, left for y); `position` moves the **tree**
  only. With `"top"` the tree is drawn above the panel, and in a
  facetted plot it is placed between the strip and the panel. On a
  [`coord_radial()`](https://rdrr.io/pkg/ggplot2/man/coord_radial.html)
  plot `"bottom"` puts the x tree **inside the hole** and `"top"`
  **outside the circle**; the r-axis tree always sits at the label-free
  end of the arc, so `position` does not move it (see "Radial
  coordinates" below).

- order, theme:

  Passed on to
  [`ggplot2::guide_axis()`](https://rdrr.io/pkg/ggplot2/man/guide_axis.html).

- ...:

  Further arguments passed to
  [`ggplot2::scale_x_discrete()`](https://rdrr.io/pkg/ggplot2/man/scale_discrete.html)
  /
  [`ggplot2::scale_y_discrete()`](https://rdrr.io/pkg/ggplot2/man/scale_discrete.html).

- ring_gap:

  Gap in centimetres between the circle and the leaf ring of a
  [`coord_radial()`](https://rdrr.io/pkg/ggplot2/man/coord_radial.html)
  theta-axis dendrogram. The default `NULL` measures the theta labels
  and puts the leaf ring right after the longest one; supply a number to
  override that.

- tree_position:

  Optional alias for `position`, kept for scripts written before
  `position` was redefined to move only the tree.

- strip_side:

  Where the tree sits relative to the facet strip when the tree is on
  the strip side (an x axis with `position = "top"`): `"below"` (the
  default) slides it between the strip and the panel, `"above"` puts it
  outside the strip. Ignored when the tree is not next to a strip.

- name, labels, limits, expand, breaks:

  Arguments passed on to
  [`ggplot2::scale_x_discrete()`](https://rdrr.io/pkg/ggplot2/man/scale_discrete.html)
  /
  [`ggplot2::scale_y_discrete()`](https://rdrr.io/pkg/ggplot2/man/scale_discrete.html).
  By default `breaks` is the dendrogram leaf order, which is what
  re-orders the axis.

- guide:

  A guide. By default a matching `guide_axis_cluster()` is created from
  the same clustering.

- value:

  Optional column name to cluster on, when the plot maps several numeric
  aesthetics and the automatic choice (first of `fill`, `colour`,
  `size`, `linewidth`, `alpha`, `weight`) is not the intended one. Only
  `scale_x_cluster()` / `scale_y_cluster()` use it, because the matrix
  is derived from the plot data.

- .map_data:

  Internal, same as `map_data`; kept on the scale signatures so the
  derived long frame reaches the guide.

## Value

`guide_axis_cluster()` returns a guide object; `scale_x_cluster()` /
`scale_y_cluster()` return a discrete ggplot2 scale.

## Details

The dendrogram is computed with
[`stats::dist()`](https://rdrr.io/r/stats/dist.html) (Euclidean
distance) followed by
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html). Rows
containing missing values are dropped with a warning before clustering.

## Tree placement

`position` only decides **where the tree is drawn**, never where the
labels go. The labels always stay on their usual side of the panel, so
the tree can sit on the opposite side without moving anything the reader
is used to.

- `"bottom"` / `"top"` (x axis): below or above the labels. With facets
  the tree is moved into the strip band, i.e. directly **under the
  strip** and above the panel, so strip, tree, labels and panel never
  overlap. Use `strip_side = "above"` to draw it outside the strip
  instead; either way a small gap (`options(ggutils.tree_gap = 0.14)`,
  in centimetres) keeps the branches from touching the strip.

- `"left"` / `"right"` (y axis): left of the labels, or at the far end
  of the panel row. With facets, `"left"` draws one tree per *visible* y
  axis; `"right"` draws one at the end of every panel row.

A tree that has to sit away from the labels cannot travel through the
axis guide (a discrete secondary axis cannot carry a free panel's
categories), so it is drawn straight into the finished gtable, where the
panel geometry is known. That is also what makes
[`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html) work:
the row facet owns the y axis and the column facet owns the x axis, and
each tree is attached to the strip of its own cell.

## Facets and `scales`

The `scales` argument of
[`facet_wrap()`](https://rdrr.io/pkg/ggplot2/man/facet_wrap.html) /
[`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html) is
read automatically, so `free` never has to be written by hand:

- `"fixed"`: one shared tree for every panel.

- `"free_x"`: each panel column gets its own x tree.

- `"free_y"`: y trees are recomputed per panel row.

- `"free"`: both axes are recomputed panel by panel.

[`facet_wrap()`](https://rdrr.io/pkg/ggplot2/man/facet_wrap.html) draws
one x tree per panel row (each under its own strip) and one y tree per
visible y axis;
[`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html) draws
one x tree per panel and one y tree per row. Writing `free = FALSE`
forces a single shared tree, `free = TRUE` forces per-panel trees
regardless of `scales`.

## Free facets

`facet_wrap(scales = "free")` gives every panel its own set of
categories, and with `free = TRUE` its own tree. A discrete scale's
limits are global, so the categories are re-keyed per panel internally:
each panel's focal column becomes `"<panel>__ggcl__<category>"` and is
ordered by that panel's own
[`hclust()`](https://rdrr.io/r/stats/hclust.html) leaf order. The axis
therefore really is in leaf order, a standard tree can be drawn without
a crossing branch, and the prefix never reaches the printed labels.
Users never see the keys.

With an explicitly supplied matrix (`data = mat`) the axis stays in data
order and the panel tree falls back to merging only **adjacent**
clusters, which keeps the layout planar at the cost of a shape that
differs from a plain [`hclust()`](https://rdrr.io/r/stats/hclust.html).

## Deriving the matrix from the plot

`scale_x_cluster()` and `scale_y_cluster()` do not need a matrix. When
`data` is omitted, the matrix is built from the plot data at draw time:

1.  the focal axis (`x` for `scale_x_cluster()`, `y` for
    `scale_y_cluster()`) provides the rows, the other positional axis
    provides the columns;

2.  the values come from the first numeric aesthetic mapped among
    `fill`, `colour`, `size`, `linewidth`, `alpha` and `weight`; use the
    `value` argument to pick another column;

3.  with no numeric aesthetic, a continuous other axis is summarised per
    category (`mean`, `sd`, `median`, `n`), and two categorical axes are
    clustered on how often the categories co-occur (a message says so).

    ggplot(df, aes(gene, sample, fill = value)) +
      geom_tile() +
      scale_x_cluster()                       # no matrix needed

Missing focal/other combinations are filled with the row mean instead of
dropping the category, so an incomplete grid still keeps every label on
the axis. `hclust` remains available for a pre-computed tree.

A guide on its own cannot change the order in which a scale maps
categories to positions: that order is fixed before the guide is built.
Therefore, to both draw the tree *and* re-order the axis, use
`scale_x_cluster()` / `scale_y_cluster()`. If you attach
`guide_axis_cluster()` directly with `guides(x = ...)`, the tree is
aligned to the categories but the data order is left unchanged.

## Radial coordinates ([`coord_radial()`](https://rdrr.io/pkg/ggplot2/man/coord_radial.html))

On a circular plot the theta (x) and r (y) axes are handled differently.

**theta axis (x) – ring dendrogram.** The category labels keep their
stock
[`guide_axis_theta()`](https://rdrr.io/pkg/ggplot2/man/guide_axis_theta.html)
look and the tree becomes a *ring dendrogram*: leaves on a circle, every
merge an arc at another radius, joined to its children by radial stubs.
`type = "triangle"` draws straight chords to each merge instead of arcs.

- `position = "bottom"` (default) draws the tree **inside the hole**:
  the leaves sit on the inner circle and every merge steps towards the
  centre. This needs `inner.radius > 0`; a pie without a hole has no
  room, so the tree is skipped.

- `position = "top"` draws it **outside the circle**: leaves on the
  outer circle, merges growing outwards. With the default
  `ring_gap = NULL` the leaf ring starts after the longest theta label,
  and extra padding is inserted around every panel, so facet strips and
  neighbouring panels are never touched. Leave some `plot.margin` /
  device room for the tree.

**r axis (y) – partial arcs only.** On a full circle the r axis is a
plain column, so `scale_y_cluster()` draws **no** tree there. With a
partial arc (`start`/`end`) ggplot2 puts the r labels on a radial ray
and the tree becomes an **arc-shaped wedge dendrogram** at the other end
of the arc, the side without labels: the leaf position becomes a radius
and the merge height an angle, so every straight branch becomes a
concentric arc and the tree hugs the heat map. `height` sets the angular
width (`min(pi/3, 0.30 * height)` radians).

**Facets.** Every panel gets its own tree, and with
`facet_wrap(scales = "free_x")` that tree is clustered from the panel's
own genes, so the panels differ. With fixed scales one shared tree is
used.

    # per-sample z-scores: standardise every sample across the genes once
    z <- local({
      m <- tapply(airway_heatmap$df$z,
                  list(airway_heatmap$df$gene, airway_heatmap$df$sample), mean)
      m <- scale(m)                                # column-wise = per sample
      out <- as.data.frame(as.table(m))
      names(out) <- c("gene", "sample", "z_sample")
      merge(out, airway_heatmap$genes[c("gene", "gene_set")], by = "gene")
    })
    d <- droplevels(z[z$gene %in% unique(z$gene)[1:16], ])
    fill_scale <- scale_fill_gradient2(low = "#2166ac", mid = "white",
                                       high = "#b2182b", midpoint = 0,
                                       name = "z (sample)")

    # (1) x tree inside the hole: bottom + inner.radius
    ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
      coord_radial(theta = "x", inner.radius = 0.55) +
      scale_x_cluster(method = "ward.D2", height = 1.2) +
      fill_scale

    # (2) x tree outside the circle: position = "top"
    ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
      coord_radial(theta = "x") +
      scale_x_cluster(method = "ward.D2", height = 1, position = "top") +
      fill_scale

    # (3) partial arc + r-axis tree at the label-free end of the arc
    ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
      coord_radial(theta = "x", start = -0.5 * pi, end = 0.5 * pi,
                   inner.radius = 0.55) +
      scale_y_cluster(method = "ward.D2", height = 1.6) +
      fill_scale

    # (4) facetted: one tree per panel, clustered per panel
    ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
      facet_wrap(~ gene_set, scales = "free_x") +
      coord_radial(theta = "x", inner.radius = 0.45) +
      scale_x_cluster(method = "ward.D2", height = 0.8) +
      fill_scale +
      theme_grey(base_size = 7)

## Faceted free scales

With `facet_wrap(scales = "free_x")` (or `"free_y"`) every panel has its
own categories, so one pre-computed tree cannot fit all panels. In that
case pass the clustering data to the guide itself and set `free = TRUE`:

    ggplot(d, aes(gene, value)) +
      geom_point() +
      facet_wrap(~ cluster, scales = "free_x") +
      guides(x = guide_axis_cluster(data = mat, free = TRUE, method = "average"))

The guide then takes, in each panel, the rows of `mat` whose names match
that panel's categories, clusters that subset, and draws a tree sized
for exactly those leaves. When the matrix was derived from the plot data
(no `data` argument), the per-panel matrix is rebuilt from that long
data, so combinations missing from a panel cannot become `NA` and drop a
category. The scale must leave the category order to the data (do
**not** set global `limits`); if you also want the categories re-ordered
per panel, use `scale_x_cluster()`/`scale_y_cluster()` with
`free = TRUE`, which re-orders each panel's axis into that panel's leaf
order for you.

## Canvas height

The tree row is a **fixed** `height` (in cm), while panel rows are
`1null`. When the fixed rows (trees + rotated axis labels + strips) need
more room than the device provides, gtable gives the null panel rows
zero height and the figure looks like trees and axis text only. Leave
roughly 2-3 cm of height per panel row – with two rows of faceted panels
plus rotated labels, a `fig.height` of 4 inches is already too short.
Either enlarge the device or lower `height`.

## See also

[`ggplot2::guide_axis()`](https://rdrr.io/pkg/ggplot2/man/guide_axis.html),
[`ggplot2::scale_x_discrete()`](https://rdrr.io/pkg/ggplot2/man/scale_discrete.html)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

set.seed(1)
mat <- matrix(rnorm(12 * 5), nrow = 12,
              dimnames = list(paste0("g", 1:12), paste0("s", 1:5)))
long <- as.data.frame(as.table(mat))
names(long) <- c("gene", "sample", "value")

# x axis: genes are clustered and re-ordered, tree above the labels
ggplot(long, aes(gene, sample, fill = value)) +
  geom_tile() +
  scale_x_cluster(mat, method = "average", height = 1.2, col = "#b2182b") +
  scale_y_discrete(guide = guide_axis(show.tick.label = TRUE))

# y axis: same matrix, tree to the right of the labels
ggplot(long, aes(sample, gene, fill = value)) +
  geom_tile() +
  scale_y_cluster(mat, method = "ward.D2", height = 1.2, col = "#2166ac")

# No matrix at all: the tree is clustered from the plot data itself
ggplot(long, aes(gene, sample, fill = value)) +
  geom_tile() +
  scale_x_cluster(method = "average", position = "top")

# Tree away from the labels: x labels stay at the bottom, the tree moves
# under the strip; y labels stay on the left, the tree moves to the right
ggplot(long, aes(gene, sample, fill = value)) +
  geom_point() +
  facet_wrap(~ sample, scales = "free_x") +
  scale_x_cluster(position = "top", height = 1)

ggplot(long, aes(gene, sample, fill = value)) +
  geom_point() +
  facet_wrap(~ sample, scales = "free_y") +
  scale_y_cluster(position = "left", height = 1)

# Both axes at once; `scales` is picked up from the facet
ggplot(long, aes(gene, sample, fill = value)) +
  geom_point() +
  facet_wrap(~ sample, scales = "free") +
  scale_x_cluster(position = "top", height = 1) +
  scale_y_cluster(position = "left", height = 1)

# facet_grid: the row facet owns y, the column facet owns x.
# Group both axes so the grid stays small; drop = FALSE keeps the empty
# levels visible, which is what makes "fixed" and "free_y" differ.
grid_df <- long
grid_df$g_half <- factor(ifelse(grid_df$gene %in% paste0("g", 1:6),
                                "g1-g6", "g7-g12"))
grid_df$s_half <- factor(ifelse(grid_df$sample %in% c("s1", "s2"),
                                "s1-s2", "s3-s5"))
ggplot(grid_df, aes(gene, sample, fill = value)) +
  geom_point() +
  facet_grid(s_half ~ g_half, scales = "free", drop = FALSE) +
  scale_x_cluster(position = "top", height = 1) +
  scale_y_cluster(position = "left", height = 1)
} # }
```
