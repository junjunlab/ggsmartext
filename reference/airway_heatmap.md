# Processed RNA-seq expression matrix (GEO GSE52778)

A small, ready-to-plot expression matrix taken from a real RNA-seq
experiment, so that examples and vignettes can start from processed data
instead of re-running the preprocessing on every render.

## Usage

``` r
airway_heatmap
```

## Format

A list with six components:

- `z`:

  numeric matrix, 300 genes x 8 samples: row z-scores, rows in cluster
  order; rows are named by gene symbol, columns by
  `<cell line>_<treatment>`.

- `genes`:

  data frame with `gene`, `gene_set` (`"up in dexamethasone"` or
  `"down in dexamethasone"`) and `delta` (mean z-score in treated minus
  untreated samples).

- `samples`:

  data frame with `sample`, `cell` and `dex` columns.

- `df`:

  long data frame ready for `ggplot2`: `gene`, `sample`, `gene_set` and
  `z`. `gene` is a factor whose levels run in reverse cluster order, so
  `geom_tile()` draws the first cluster row at the top.

- `mark`:

  character vector of 20 gene names used by the single-panel example (12
  genes from the top response cluster plus 8 scattered genes).

- `mark_by_set`:

  named list with the four strongest responders of each gene set, used
  by the faceted example.

## Source

[doi:10.1371/journal.pone.0099625](https://doi.org/10.1371/journal.pone.0099625)
(airway RNA-seq, GEO GSE52778).

## Details

The experiment profiled human airway smooth muscle cells: four cell
lines, each treated with dexamethasone or left untreated (8 samples in
total). The original read counts come from the `airway` Bioconductor
package (GEO series
[GSE52778](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE52778),
Himes et al. 2014, PLoS ONE 9(6): e99625). The object bundled here was
prepared once with the following steps:

1.  keep genes expressed in at least four samples (CPM \> 1);

2.  `log2(CPM + 1)` transform;

3.  drop duplicated gene symbols so every label is unique;

4.  keep the 300 most variable genes and z-score each gene across
    samples;

5.  order the genes by hierarchical clustering
    (`hclust(dist(z), method = "ward.D2")`).

## Examples

``` r
str(airway_heatmap, max.level = 1)
#> List of 6
#>  $ z          : num [1:300, 1:8] 0.161 0.572 0.544 0.814 0.567 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ genes      :'data.frame': 300 obs. of  3 variables:
#>  $ samples    :'data.frame': 8 obs. of  3 variables:
#>  $ df         :'data.frame': 2400 obs. of  4 variables:
#>  $ mark       : chr [1:20] "EBF3" "PCDHGA12" "RUNX3" "DCLK1" ...
#>  $ mark_by_set:List of 2
dim(airway_heatmap$z)
#> [1] 300   8
head(airway_heatmap$df)
#>       gene       sample              gene_set         z
#> 1     EBF3 N61311_untrt down in dexamethasone 0.1610692
#> 2 PCDHGA12 N61311_untrt down in dexamethasone 0.5721993
#> 3    RUNX3 N61311_untrt   up in dexamethasone 0.5444419
#> 4    DCLK1 N61311_untrt down in dexamethasone 0.8143485
#> 5     RHOU N61311_untrt down in dexamethasone 0.5673247
#> 6  ALDH1A1 N61311_untrt down in dexamethasone 0.2585896
```
