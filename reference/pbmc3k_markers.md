# PBMC3k single-cell marker table

The marker table from the Seurat PBMC3k tutorial: for each of nine cell
clusters, the top 20 marker genes with their fold change and expression
fractions. Bundled so that clustering examples start from a real
single-cell marker matrix without downloading anything.

## Usage

``` r
pbmc3k_markers
```

## Format

A list with two components:

- `markers`:

  data frame (180 rows) with `cluster_id` (0–8), `celltype` (factor, 9
  levels), `gene`, `avg_log2FC`, `pct.1`, `pct.2`, `p_val` and
  `p_val_adj`.

- `mat`:

  numeric matrix, 177 genes x 9 cell types of `avg_log2FC`; genes absent
  from a cell type are zero. Intended as the input to
  [`scale_x_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md)
  /
  [`scale_y_cluster()`](https://junjunlab.github.io/ggutils/reference/guide_axis_cluster.md).

## Source

Seurat PBMC3k tutorial marker table (`pbmc3k_markers.csv`).

## Examples

``` r
str(pbmc3k_markers, max.level = 1)
#> List of 2
#>  $ markers:'data.frame': 180 obs. of  8 variables:
#>  $ mat    : num [1:177, 1:9] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..- attr(*, "dimnames")=List of 2
dim(pbmc3k_markers$mat)
#> [1] 177   9
```
