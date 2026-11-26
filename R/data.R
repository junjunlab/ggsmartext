#' Processed RNA-seq expression matrix (GEO GSE52778)
#'
#' A small, ready-to-plot expression matrix taken from a real RNA-seq
#' experiment, so that examples and vignettes can start from processed data
#' instead of re-running the preprocessing on every render.
#'
#' The experiment profiled human airway smooth muscle cells: four cell lines,
#' each treated with dexamethasone or left untreated (8 samples in total).
#' The original read counts come from the `airway` Bioconductor package
#' (GEO series [GSE52778](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE52778),
#' Himes et al. 2014, PLoS ONE 9(6): e99625). The object bundled here was
#' prepared once with the following steps:
#'
#' 1. keep genes expressed in at least four samples (CPM > 1);
#' 2. `log2(CPM + 1)` transform;
#' 3. drop duplicated gene symbols so every label is unique;
#' 4. keep the 300 most variable genes and z-score each gene across samples;
#' 5. order the genes by hierarchical clustering
#'    (`hclust(dist(z), method = "ward.D2")`).
#'
#' @format A list with six components:
#' \describe{
#'   \item{\code{z}}{numeric matrix, 300 genes x 8 samples: row z-scores,
#'     rows in cluster order; rows are named by gene symbol, columns by
#'     \code{<cell line>_<treatment>}.}
#'   \item{\code{genes}}{data frame with \code{gene}, \code{gene_set}
#'     (\code{"up in dexamethasone"} or \code{"down in dexamethasone"}) and
#'     \code{delta} (mean z-score in treated minus untreated samples).}
#'   \item{\code{samples}}{data frame with \code{sample}, \code{cell} and
#'     \code{dex} columns.}
#'   \item{\code{df}}{long data frame ready for \code{ggplot2}: \code{gene},
#'     \code{sample}, \code{gene_set} and \code{z}. \code{gene} is a factor
#'     whose levels run in reverse cluster order, so \code{geom_tile()} draws
#'     the first cluster row at the top.}
#'   \item{\code{mark}}{character vector of 20 gene names used by the
#'     single-panel example (12 genes from the top response cluster plus 8
#'     scattered genes).}
#'   \item{\code{mark_by_set}}{named list with the four strongest responders
#'     of each gene set, used by the faceted example.}
#' }
#'
#' @source \doi{10.1371/journal.pone.0099625} (airway RNA-seq, GEO GSE52778).
#'
#' @examples
#' str(airway_heatmap, max.level = 1)
#' dim(airway_heatmap$z)
#' head(airway_heatmap$df)
"airway_heatmap"

#' PBMC3k single-cell marker table
#'
#' The marker table from the Seurat PBMC3k tutorial: for each of nine cell
#' clusters, the top 20 marker genes with their fold change and expression
#' fractions. Bundled so that clustering examples start from a real single-cell
#' marker matrix without downloading anything.
#'
#' @format A list with two components:
#' \describe{
#'   \item{\code{markers}}{data frame (180 rows) with \code{cluster_id}
#'     (0--8), \code{celltype} (factor, 9 levels), \code{gene},
#'     \code{avg_log2FC}, \code{pct.1}, \code{pct.2}, \code{p_val} and
#'     \code{p_val_adj}.}
#'   \item{\code{mat}}{numeric matrix, 177 genes x 9 cell types of
#'     \code{avg_log2FC}; genes absent from a cell type are zero. Intended as the
#'     input to \code{scale_x_cluster()} / \code{scale_y_cluster()}.}
#' }
#' @source Seurat PBMC3k tutorial marker table (\code{pbmc3k_markers.csv}).
#'
#' @examples
#' str(pbmc3k_markers, max.level = 1)
#' dim(pbmc3k_markers$mat)
"pbmc3k_markers"

#' PBMC3k dot plot data
#'
#' A complete gene-by-cluster table from the Seurat PBMC3k tutorial, shaped the
#' way a dot plot needs it: every one of the 90 genes is measured in every one
#' of the nine cell types (810 rows, no gaps), so a facetted plot has the same
#' genes in every panel and a single-panel plot can show the whole matrix.
#'
#' @format A data frame with 810 rows and six columns:
#' \describe{
#'   \item{\code{cluster_id}}{integer, Seurat cluster 0--8.}
#'   \item{\code{celltype}}{factor with 9 levels, ordered by \code{cluster_id}.}
#'   \item{\code{gene}}{character, 90 unique gene symbols.}
#'   \item{\code{avg_expression}}{numeric, average expression in the cluster
#'     (log-normalised counts).}
#'   \item{\code{pct_expressed}}{numeric, percentage of cells in the cluster
#'     that express the gene (0--100), i.e. the dot size in a dot plot.}
#'   \item{\code{avg_exp_scaled}}{numeric, z-score of \code{avg_expression}
#'     across cell types, i.e. the dot colour in a dot plot.}
#' }
#' @source Seurat PBMC3k tutorial (\code{pbmc3k_dotplot_data.csv}).
#'
#' @examples
#' str(pbmc3k_dotplot)
#' length(unique(pbmc3k_dotplot$gene))
#'
#' # the 3 strongest genes of each cell type, for a compact example
#' top3 <- lapply(split(pbmc3k_dotplot, pbmc3k_dotplot$celltype), function(d) {
#'   head(d[order(-d$avg_exp_scaled), "gene"], 3)
#' })
#' unique(unlist(top3, use.names = FALSE))
"pbmc3k_dotplot"
