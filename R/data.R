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
