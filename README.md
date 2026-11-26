
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggutils

[![R-CMD-check](https://github.com/junjunlab/ggutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/junjunlab/ggutils/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R \>=
4.1](https://img.shields.io/badge/R-%3E%3D%204.1-blue.svg)](https://cran.r-project.org/)
[![ggplot2 \>=
4.0](https://img.shields.io/badge/ggplot2-%3E%3D%204.0-blueviolet.svg)](https://ggplot2.tidyverse.org/)
[![Docs](https://img.shields.io/badge/docs-pkgdown-2E7D6B.svg)](https://junjunlab.github.io/ggutils/)
[![Last
commit](https://img.shields.io/github/last-commit/junjunlab/ggutils.svg)](https://github.com/junjunlab/ggutils/commits/main)

> Small, reusable utilities for `ggplot2`.

`ggutils` adds helpers that plug into the usual grammar – no new
geometry, no special data format.

## `guide_axis_smart()`: readable labels on crowded axes

Picks the labels you want, spreads them over several rows / columns so
they never overlap, and draws a leader line from every label back to its
tick.

``` r
ggplot(d, aes(gene, value)) + geom_col() +
  guides(x = guide_axis_smart(n = 8))
```

## `guide_axis_cluster()`: dendrograms on the axes

Clusters the categories, orders the axis by the dendrogram and draws the
tree next to it. `position` moves the tree only; free facets get one
tree per panel; on `coord_radial()` it becomes a ring tree.

``` r
ggplot(d, aes(gene, sample, fill = z)) + geom_tile() +
  scale_x_cluster(method = "ward.D2") +
  scale_y_cluster(position = "left")
```

## `facet_gridn()`: per-panel free scales for `facet_grid()`

``` r
ggplot(d, aes(gene, value)) + geom_point() +
  facet_gridn(rows = vars(cluster), cols = vars(set), scales = "free_x")
```

## `coord_spiral()`: Cartesian plots on a spiral

Bends a normal ggplot into an Archimedean spiral and draws the angle and
radial axes for you.

``` r
ggplot(d, aes(x, y)) + geom_line() + coord_spiral(turns = 3)
```

## Installation

Requires R \>= 4.1 and **ggplot2 \>= 4.0.0**.

``` r
# install.packages("remotes")
remotes::install_github("junjunlab/ggutils")
```

## Documentation

- **Reference manual**: <https://junjunlab.github.io/ggutils/reference/>
- **中文 vignette**:
  <https://junjunlab.github.io/ggutils/articles/ggutils.html>
- **English vignette**:
  <https://junjunlab.github.io/ggutils/articles/ggutils-en.html>

## Credits

The overlap-free label compaction is a port of `smartAlign` from
**circlize**; the box arrangement is a port from **ComplexHeatmap**. The
ggplot2 guide integration is new.
