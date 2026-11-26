# A `facet_grid()` that can free scales panel by panel

`facet_gridn()` lays panels out exactly like
[`ggplot2::facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html)
(rows and columns of facets, row strips on the right, column strips on
top), but `scales = "free_x"` / `"free_y"` / `"free"` free the axis
**for every single panel** instead of once per column / row. Each panel
then trains its own scale on its own data, gets its own axis range and
its own axis labels.

## Usage

``` r
facet_gridn(
  rows = NULL,
  cols = NULL,
  scales = "fixed",
  space = "fixed",
  shrink = TRUE,
  labeller = "label_value",
  as.table = TRUE,
  switch = NULL,
  drop = TRUE,
  margins = FALSE,
  axes = "margins",
  axis.labels = "all"
)

FacetGridN
```

## Format

An object of class `FacetGridN` (inherits from `FacetGrid`, `Facet`,
`ggproto`, `gg`) of length 3.

## Arguments

- rows, cols:

  A set of variables or expressions quoted by
  [`ggplot2::vars()`](https://rdrr.io/pkg/ggplot2/man/vars.html) and
  defining faceting groups on the rows or columns dimension.

- scales:

  One of `"fixed"` (default), `"free_x"`, `"free_y"` or `"free"` for
  **per-panel** freedom, or `"free_x_bycol"` / `"free_y_byrow"` for the
  stock
  [`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html)
  behaviour (x per column, y per row).

- space:

  If `"fixed"`, all panels have the same size. If `"free_x"`, `"free_y"`
  or `"free"`, panel size is proportional to the panel's own range on
  that axis (this pairs naturally with per-panel scales).

- shrink:

  If `TRUE`, will shrink scales to fit output of statistics, not raw
  data. Passed on to
  [`ggplot2::facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html).

- labeller:

  A function that takes one data frame of labels and returns a list or
  data frame of character vectors.

- as.table:

  If `TRUE` (the default), facets are laid out like a table, with the
  highest value in the bottom-right corner.

- switch:

  By default, the labels are displayed on the top and right of the plot.
  Set to `"x"`, `"y"` or `"both"` to move them.

- drop:

  If `TRUE` (the default), unused factor levels are dropped.

- margins:

  Passed on to
  [`ggplot2::facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html).

- axes, axis.labels:

  Where to draw axes and labels, as in
  [`ggplot2::facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html).
  The defaults (`"margins"`) are what
  [`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html)
  uses; when an axis is freed **per panel** the matching axes are drawn
  for every panel regardless, because one axis per row / column could
  not label the other cells.

## Value

A `FacetGridN` ggproto object, usable in the same places as
[`ggplot2::facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html).

## Details

The two stock behaviours are still available as `"free_x_bycol"` and
`"free_y_byrow"`: they free x per column / y per row exactly like
[`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html), which
is handy when one figure has to mix both kinds of freedom.

## How it differs from [`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html)

`facet_grid(scales = "free_x")` frees x per **column**: panels stacked
in the same column still share one x scale, because the layout assigns
`SCALE_X = COL`. `facet_gridn()` assigns `SCALE_X = PANEL`, so every
cell of the grid is independent:

|  |  |  |  |
|----|----|----|----|
| `scales` | `facet_gridn()` | x 刻度 | y 刻度 |
| `"fixed"` | 共享（默认） | 全局一条 | 全局一条 |
| `"free_x"` | **逐面板** | 每个面板一条 | 全局一条 |
| `"free_y"` | **逐面板** | 全局一条 | 每个面板一条 |
| `"free"` | **逐面板** | 每个面板一条 | 每个面板一条 |
| `"free_x_bycol"` | 同 [`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html) | 每列一条 | 全局一条 |
| `"free_y_byrow"` | 同 [`facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html) | 全局一条 | 每行一条 |

Because a panel's range is its own, panels are no longer comparable
through a shared axis: read each panel's own axis labels. `"fixed"` is
unchanged, so a figure can switch between the two without touching
anything else.

## See also

[`ggplot2::facet_grid()`](https://rdrr.io/pkg/ggplot2/man/facet_grid.html),
[`ggplot2::facet_wrap()`](https://rdrr.io/pkg/ggplot2/man/facet_wrap.html)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Continuous: each panel gets its own x AND y range
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_gridn(vars(cyl), vars(gear), scales = "free")

# Same data with the stock facet: x is shared per column
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  facet_grid(vars(cyl), vars(gear), scales = "free_x")

# Categorical axes: each panel keeps only its own categories
ggplot(mpg, aes(class, hwy)) +
  geom_boxplot() +
  facet_gridn(vars(drv), vars(year), scales = "free_x")
} # }
```
