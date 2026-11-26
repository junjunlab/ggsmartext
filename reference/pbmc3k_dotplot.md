# PBMC3k dot plot data

A complete gene-by-cluster table from the Seurat PBMC3k tutorial, shaped
the way a dot plot needs it: every one of the 90 genes is measured in
every one of the nine cell types (810 rows, no gaps), so a facetted plot
has the same genes in every panel and a single-panel plot can show the
whole matrix.

## Usage

``` r
pbmc3k_dotplot
```

## Format

A data frame with 810 rows and six columns:

- `cluster_id`:

  integer, Seurat cluster 0–8.

- `celltype`:

  factor with 9 levels, ordered by `cluster_id`.

- `gene`:

  character, 90 unique gene symbols.

- `avg_expression`:

  numeric, average expression in the cluster (log-normalised counts).

- `pct_expressed`:

  numeric, percentage of cells in the cluster that express the gene
  (0–100), i.e. the dot size in a dot plot.

- `avg_exp_scaled`:

  numeric, z-score of `avg_expression` across cell types, i.e. the dot
  colour in a dot plot.

## Source

Seurat PBMC3k tutorial (`pbmc3k_dotplot_data.csv`).

## Examples

``` r
str(pbmc3k_dotplot)
#> 'data.frame':    810 obs. of  6 variables:
#>  $ cluster_id    : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ celltype      : Factor w/ 9 levels "Naive CD4 T",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ gene          : chr  "CCR7" "LEF1" "MAL" "PRKCQ-AS1" ...
#>  $ avg_expression: num  2.98 2.1 1.82 2.1 2.29 ...
#>  $ pct_expressed : num  43.6 33.6 26.3 33.1 38.5 ...
#>  $ avg_exp_scaled: num  2.08 2.03 1.84 1.82 1.76 ...
length(unique(pbmc3k_dotplot$gene))
#> [1] 90

# the 3 strongest genes of each cell type, for a compact example
top3 <- lapply(split(pbmc3k_dotplot, pbmc3k_dotplot$celltype), function(d) {
  head(d[order(-d$avg_exp_scaled), "gene"], 3)
})
unique(unlist(top3, use.names = FALSE))
#>  [1] "CCR7"          "LEF1"          "MAL"           "FOLR3"        
#>  [5] "S100A12"       "CD14"          "CD40LG"        "AQP3"         
#>  [9] "TRADD"         "LINC00926"     "VPREB3"        "CD79A"        
#> [13] "GZMK"          "CD8A"          "KLRG1"         "CKB"          
#> [17] "CDKN1C"        "MS4A4A"        "AKR1C3"        "SH2D1B"       
#> [21] "SPON2"         "SERPINF1"      "FCER1A"        "CLIC2"        
#> [25] "LY6G6F"        "RP11-879F14.2" "CLDN5"        
```
