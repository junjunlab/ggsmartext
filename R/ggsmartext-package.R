#' ggsmartext: Smart Axis Labels for ggplot2
#'
#' When a discrete axis of a `ggplot2` plot carries more category labels than it
#' can display, `guide_axis_smart()` shows only the labels you ask for, connects
#' every displayed label to its tick with a leader line and spreads the labels
#' over several rows (or columns) so that the available page space is used
#' efficiently.
#'
#' The one-dimensional arrangement is a port of the `smartAlign` algorithm used
#' by the [circlize](https://github.com/jokergoo/circlize) package, and of the
#' box arrangement used by the
#' [ComplexHeatmap](https://github.com/jokergoo/ComplexHeatmap) package.
#'
#' @section Main functions:
#' * [guide_axis_smart()] -- the smart axis guide.
#' * [smart_align()] -- one-dimensional, overlap-free arrangement of labelled
#'   boxes (pure `smartAlign` port).
#' * [box_arrange()] -- cluster based arrangement of labelled boxes.
#' * [smart_stack()] -- multi-row arrangement of labels along one axis.
#'
#' @keywords internal
#' @importFrom grid unit unit.c gpar gList gTree setChildren textGrob
#'   segmentsGrob convertWidth convertHeight stringWidth stringHeight
#'   grobWidth grobHeight viewport pushViewport popViewport nullGrob
#'   makeContent
"_PACKAGE"

#' @importFrom rlang `%||%`
#' @keywords internal
NULL

## ---------------------------------------------------------------------------
## Package state
## ---------------------------------------------------------------------------

## Registered in .onLoad(); see zzz.R
.ggse_env <- new.env(parent = emptyenv())
