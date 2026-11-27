#' A `facet_grid()` that can free scales panel by panel
#'
#' `facet_gridn()` lays panels out exactly like [ggplot2::facet_grid()] (rows and
#' columns of facets, row strips on the right, column strips on top), but
#' `scales = "free_x"` / `"free_y"` / `"free"` free the axis **for every single
#' panel** instead of once per column / row.  Each panel then trains its own
#' scale on its own data, gets its own axis range and its own axis labels.
#'
#' The two stock behaviours are still available as `"free_x_bycol"` and
#' `"free_y_byrow"`: they free x per column / y per row exactly like
#' `facet_grid()`, which is handy when one figure has to mix both kinds of
#' freedom.
#'
#' @section How it differs from `facet_grid()`:
#' `facet_grid(scales = "free_x")` frees x per **column**: panels stacked in the
#' same column still share one x scale, because the layout assigns
#' `SCALE_X = COL`.  `facet_gridn()` assigns `SCALE_X = PANEL`, so every cell of
#' the grid is independent:
#'
#' | `scales` | `facet_gridn()` | x scale | y scale |
#' |---|---|---|---|
#' | `"fixed"` | shared (default) | one global | one global |
#' | `"free_x"` | **per panel** | one per panel | one global |
#' | `"free_y"` | **per panel** | one global | one per panel |
#' | `"free"` | **per panel** | one per panel | one per panel |
#' | `"free_x_bycol"` | same as `facet_grid()` | one per column | one global |
#' | `"free_y_byrow"` | same as `facet_grid()` | one global | one per row |
#'
#' Because a panel's range is its own, panels are no longer comparable through a
#' shared axis: read each panel's own axis labels.  `"fixed"` is unchanged, so a
#' figure can switch between the two without touching anything else.
#'
#' @param rows,cols A set of variables or expressions quoted by [ggplot2::vars()]
#'   and defining faceting groups on the rows or columns dimension.
#' @param scales One of `"fixed"` (default), `"free_x"`, `"free_y"` or `"free"`
#'   for **per-panel** freedom, or `"free_x_bycol"` / `"free_y_byrow"` for the
#'   stock `facet_grid()` behaviour (x per column, y per row).
#' @param space If `"fixed"`, all panels have the same size.  If `"free_x"`,
#'   `"free_y"` or `"free"`, panel size is proportional to the panel's own range
#'   on that axis (this pairs naturally with per-panel scales).
#' @param shrink If `TRUE`, will shrink scales to fit output of statistics, not
#'   raw data.  Passed on to [ggplot2::facet_grid()].
#' @param labeller A function that takes one data frame of labels and returns a
#'   list or data frame of character vectors.
#' @param as.table If `TRUE` (the default), facets are laid out like a table,
#'   with the highest value in the bottom-right corner.
#' @param switch By default, the labels are displayed on the top and right of the
#'   plot.  Set to `"x"`, `"y"` or `"both"` to move them.
#' @param drop If `TRUE` (the default), unused factor levels are dropped.
#' @param margins Passed on to [ggplot2::facet_grid()].
#' @param axes,axis.labels Where to draw axes and labels, as in
#'   [ggplot2::facet_grid()].  The defaults (`"margins"`) are what `facet_grid()`
#'   uses; when an axis is freed **per panel** the matching axes are drawn for
#'   every panel regardless, because one axis per row / column could not label
#'   the other cells.
#'
#' @return A `FacetGridN` ggproto object, usable in the same places as
#'   [ggplot2::facet_grid()].
#' @seealso [ggplot2::facet_grid()], [ggplot2::facet_wrap()]
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Continuous: each panel gets its own x AND y range
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   facet_gridn(vars(cyl), vars(gear), scales = "free")
#'
#' # Same data with the stock facet: x is shared per column
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   facet_grid(vars(cyl), vars(gear), scales = "free_x")
#'
#' # Categorical axes: each panel keeps only its own categories
#' ggplot(mpg, aes(class, hwy)) +
#'   geom_boxplot() +
#'   facet_gridn(vars(drv), vars(year), scales = "free_x")
#' }
#' @export
facet_gridn <- function(rows = NULL, cols = NULL, scales = "fixed",
                        space = "fixed", shrink = TRUE,
                        labeller = "label_value", as.table = TRUE,
                        switch = NULL, drop = TRUE, margins = FALSE,
                        axes = "margins", axis.labels = "all") {
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  scales <- rlang::arg_match0(
    scales %||% "fixed",
    c("fixed", "free_x", "free_y", "free", "free_x_bycol", "free_y_byrow")
  )
  ## `free` 决定"这个轴是否自由"（交给 ggplot2 的通用机制），
  ## `per_panel` 决定"自由到面板还是到列/行"（本 facet 自己的部分）。
  free <- list(
    x = any(scales %in% c("free_x", "free", "free_x_bycol")),
    y = any(scales %in% c("free_y", "free", "free_y_byrow"))
  )
  per_panel <- list(
    x = any(scales %in% c("free_x", "free")),
    y = any(scales %in% c("free_y", "free"))
  )
  space <- rlang::arg_match0(space %||% "fixed", c("fixed", "free_x", "free_y", "free"))
  space_free <- list(
    x = any(space %in% c("free_x", "free")),
    y = any(space %in% c("free_y", "free"))
  )
  draw_axes <- rlang::arg_match0(axes, c("margins", "all_x", "all_y", "all"))
  draw_axes <- list(
    x = any(draw_axes %in% c("all_x", "all")),
    y = any(draw_axes %in% c("all_y", "all"))
  )
  axis_labels <- rlang::arg_match0(axis.labels, c("margins", "all_x", "all_y", "all"))
  axis_labels <- list(
    x = !draw_axes$x || any(axis_labels %in% c("all_x", "all")),
    y = !draw_axes$y || any(axis_labels %in% c("all_y", "all"))
  )
  if (!is.null(switch)) {
    rlang::arg_match0(switch, c("both", "x", "y"))
  }
  facets_list <- ggplot2:::grid_as_facets_list(rows, cols)
  labeller <- ggplot2:::validate_labeller(labeller)

  ggplot2::ggproto(
    NULL, FacetGridN, shrink = shrink,
    params = list(
      rows = facets_list$rows, cols = facets_list$cols, margins = margins,
      free = free, per_panel = per_panel, space_free = space_free, labeller = labeller,
      as.table = as.table, switch = switch, drop = drop,
      draw_axes = draw_axes, axis_labels = axis_labels
    )
  )
}

#' @rdname facet_gridn
#' @export
FacetGridN <- ggplot2::ggproto("FacetGridN", ggplot2::FacetGrid,

  ## The layout already carries one `SCALE_X` / `SCALE_Y` per panel: the generic
  ## `Facet$init_scales()`, `train_scales()` and the per-panel `panel_params`
  ## all read those two columns.  `facet_grid()` points them at `COL` / `ROW`
  ## (one scale per column / row); pointing them at `PANEL` is what makes every
  ## cell independent.
  compute_layout = function(self, data, params) {
    layout <- ggproto_parent(ggplot2::FacetGrid, self)$compute_layout(data, params)
    if (isTRUE(params$per_panel$x)) {
      layout$SCALE_X <- as.integer(layout$PANEL)
    }
    if (isTRUE(params$per_panel$y)) {
      layout$SCALE_Y <- as.integer(layout$PANEL)
    }
    layout
  },

  ## Axes follow the scales.  A shared axis is still drawn once per margin (the
  ## `facet_grid()` look), but a free axis is drawn for *every* panel: with one
  ## scale per panel, one axis per row / column would label only the first cell
  ## of each group and leave the others without any numbers.
  attach_axes = function(self, table, layout, ranges, coord, theme, params) {
    free <- params$free %||% list(x = FALSE, y = FALSE)
    ## 只有 per_panel 为 TRUE 时才"逐面板一条轴"；`free_x_bycol` /
    ## `free_y_byrow` 仍是 stock 的按列/按行，轴留在外边距上。
    pp <- params$per_panel %||% free
    draw_axes <- params$draw_axes %||% list(x = FALSE, y = FALSE)
    axis_labels <- params$axis_labels %||% list(x = TRUE, y = TRUE)
    dim <- c(max(layout$ROW), max(layout$COL))
    n <- nrow(layout)
    panel_order <- order(layout$ROW, layout$COL)

    if (isTRUE(pp$x)) {
      x_panels <- seq_len(n)
      x_order <- as.integer(layout$PANEL[panel_order])
    } else {
      x_panels <- which(layout$ROW == 1)
      x_order <- layout$COL
    }
    if (isTRUE(pp$y)) {
      y_panels <- seq_len(n)
      y_order <- as.integer(layout$PANEL[panel_order])
    } else {
      y_panels <- which(layout$COL == 1)
      y_order <- layout$ROW
    }

    ranges <- ggplot2:::censor_labels(ranges, layout, axis_labels)
    axes <- ggplot2:::render_axes(
      ranges[x_panels], ranges[y_panels], coord, theme, transpose = TRUE
    )
    mtx <- function(x, o) matrix(x[o], dim[1], dim[2], byrow = TRUE)

    ## 逐面板轴织在面板之间（每格自带数字）；按列/按行的轴仍补在外边距上，
    ## 与 `facet_grid()` 完全一致。
    if (isTRUE(pp$x) || isTRUE(draw_axes$x)) {
      table <- ggplot2:::weave_axes(table, lapply(axes$x, mtx, o = x_order))
    } else {
      table <- ggplot2:::seam_table(
        table, axes$x$top, side = "top", name = "axis-t", z = 3
      )
      table <- ggplot2:::seam_table(
        table, axes$x$bottom, side = "bottom", name = "axis-b", z = 3
      )
    }
    if (isTRUE(pp$y) || isTRUE(draw_axes$y)) {
      table <- ggplot2:::weave_axes(table, lapply(axes$y, mtx, o = y_order))
    } else {
      table <- ggplot2:::seam_table(
        table, axes$y$left, side = "left", name = "axis-l", z = 3
      )
      table <- ggplot2:::seam_table(
        table, axes$y$right, side = "right", name = "axis-r", z = 3
      )
    }
    table
  }
)
