# Keep cluster trees inside the facet strip

[`scale_x_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
and
[`scale_y_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
mark the plot automatically, so trees attached with `position = "top"`
are moved between the strip and the panel as soon as the plot is drawn.
When the tree comes from a second guide added with
[ggplot2::guides](https://rdrr.io/pkg/ggplot2/man/guides.html) (for
example `guides(x.top = guide_axis_cluster(...))`), wrap the plot in
`strip_cluster()` once to get the same layout.

## Usage

``` r
strip_cluster(plot)
```

## Arguments

- plot:

  A `ggplot` object.

## Value

The same plot with the strip layout enabled.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- ggplot(df, aes(gene, sample)) +
  geom_tile() +
  facet_wrap(~ celltype, scales = "free") +
  guides(x.top = guide_axis_cluster(mat, free = TRUE, position = "top"))
strip_cluster(p)
} # }
```
