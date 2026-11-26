# Retrieve the dendrogram leaf order

Retrieve the dendrogram leaf order

## Usage

``` r
cluster_order(data, method = "complete")
```

## Arguments

- data:

  A numeric matrix or data frame whose rows are the categorical levels
  to cluster ([`rownames()`](https://rdrr.io/r/base/colnames.html) give
  the labels) and whose columns are the features used for clustering.
  Alternatively an `hclust` object, or — for
  [`scale_x_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  /
  [`scale_y_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  — omitted, in which case the matrix is derived from the plot's own
  data (see Details).

- method:

  Clustering method passed to
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html). One of
  `"ward.D"`, `"ward.D2"`, `"single"`, `"complete"`, `"average"`,
  `"mcquitty"`, `"median"` or `"centroid"`.

## Value

A character vector of labels in dendrogram leaf order.
