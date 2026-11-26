#' Cluster axis guide for ggplot2
#'
#' `guide_axis_cluster()` draws a hierarchical clustering dendrogram along a
#' discrete x or y axis.  The companion scales [scale_x_cluster()] and
#' [scale_y_cluster()] cluster the categories themselves, so the categories are
#' re-ordered into dendrogram leaf order and the tree is aligned to the new
#' positions automatically.
#'
#' @param data A numeric matrix or data frame whose rows are the categorical
#'   levels to cluster (`rownames()` give the labels) and whose columns are the
#'   features used for clustering.  Alternatively an `hclust` object, or —
#'   for [scale_x_cluster()] / [scale_y_cluster()] — omitted, in which case the
#'   matrix is derived from the plot's own data (see Details).
#' @param method Clustering method passed to [stats::hclust()].  One of
#'   `"ward.D"`, `"ward.D2"`, `"single"`, `"complete"`, `"average"`,
#'   `"mcquitty"`, `"median"` or `"centroid"`.
#' @param height Overall height of the dendrogram, in centimetres.  For a
#'   cartesian y axis this is the width of the tree.  On a `coord_radial()`
#'   theta axis it is the thickness of the ring dendrogram (inwards for
#'   `position = "bottom"`, outwards for `"top"`); for an r-axis tree on a
#'   partial arc it is the angular width of the wedge
#'   (`min(pi/3, 0.30 * height)` radians).
#' @param ring_gap Gap in centimetres between the circle and the leaf ring of a
#'   `coord_radial()` theta-axis dendrogram.  The default `NULL` measures the
#'   theta labels and puts the leaf ring right after the longest one; supply a
#'   number to override that.
#' @param col Colour of the dendrogram lines.
#' @param lwd Line width of the dendrogram lines.
#' @param type Shape of the dendrogram. `"rectangle"` (the default) draws
#'   rectangular branches, like `plot.dendrogram(type = "rectangle")`.
#'   `"triangle"` draws diagonal branches, like
#'   `plot.dendrogram(type = "triangle")`.
#' @param hclust An optional `hclust` object.  When supplied, `data` and
#'   `method` are ignored.  This is used internally by [scale_x_cluster()] and
#'   [scale_y_cluster()] so that the scale and the guide share one clustering.
#' @param free For use with `facet_wrap(scales = "free")`.  When `TRUE`, the
#'   dendrogram is computed **per panel** from the subset of `data` rows that
#'   match that panel's categories, so each facet gets its own tree and its own
#'   leaf order.  When `data` is omitted (see [scale_x_cluster()]) the per-panel
#'   matrix is rebuilt from the plot data instead, so no matrix is required.
#'   See the "Faceted free scales" section below.
#' @param value Optional column name to cluster on, when the plot maps several
#'   numeric aesthetics and the automatic choice (first of `fill`, `colour`,
#'   `size`, `linewidth`, `alpha`, `weight`) is not the intended one.  Only
#'   `scale_x_cluster()` / `scale_y_cluster()` use it, because the matrix is
#'   derived from the plot data.
#' @param map_data Internal.  Long frame (`.focal`, `.other`, `.value`) used to
#'   rebuild the matrix of a free-scale panel; filled in automatically by the
#'   cluster scales.
#' @param .map_data Internal, same as `map_data`; kept on the scale signatures
#'   so the derived long frame reaches the guide.
#' @param .tree_labels Internal.  The raw category names, used to match a tree's
#'   leaves when the displayed labels differ from the axis values (free facets
#'   re-key them per panel).
#' @param title Axis title, passed on to [ggplot2::guide_axis()].
#' @param position Where the dendrogram is drawn: `"bottom"`/`"top"` for an x
#'   axis, `"left"`/`"right"` for a y axis.  The category names always stay on
#'   the usual side (bottom for x, left for y); `position` moves the **tree**
#'   only.  With `"top"` the tree is drawn above the panel, and in a facetted
#'   plot it is placed between the strip and the panel.  On a
#'   `coord_radial()` plot `"bottom"` puts the x tree **inside the hole** and
#'   `"top"` **outside the circle**; the r-axis tree always sits at the
#'   label-free end of the arc, so `position` does not move it (see
#'   "Radial coordinates" below).
#' @param cluster_position Where the tree sits when the guide is attached
#'   directly with `guides()`; defaults to the axis side (`"bottom"` for an x
#'   axis, `"left"` for a y axis).
#' @param show_labels When `FALSE` the labels keep their layout but are drawn
#'   invisibly, so only the tree shows -- used when a secondary axis carries the
#'   tree while the main axis prints the category names.
#' @param draw_tree Internal switch used by [scale_x_cluster()]: `FALSE` draws
#'   only the labels, and the tree is injected into the finished gtable instead
#'   (that is what `position = "top"` needs).
#' @param tree_position Optional alias for `position`, kept for scripts written
#'   before `position` was redefined to move only the tree.
#' @param strip_side Where the tree sits relative to the facet strip when the
#'   tree is on the strip side (an x axis with `position = "top"`):
#'   `"below"` (the default) slides it between the strip and the panel,
#'   `"above"` puts it outside the strip.  Ignored when the tree is not next to
#'   a strip.
#' @param order,theme Passed on to [ggplot2::guide_axis()].
#' @param name,labels,limits,expand,breaks Arguments passed on to
#'   [ggplot2::scale_x_discrete()] / [ggplot2::scale_y_discrete()].  By default
#'   `breaks` is the dendrogram leaf order, which is what re-orders the axis.
#' @param guide A guide.  By default a matching [guide_axis_cluster()] is
#'   created from the same clustering.
#' @param ... Further arguments passed to
#'   [ggplot2::scale_x_discrete()] / [ggplot2::scale_y_discrete()].
#'
#' @details
#' The dendrogram is computed with `stats::dist()` (Euclidean distance) followed
#' by `stats::hclust()`.  Rows containing missing values are dropped with a
#' warning before clustering.
#'
#' @section Tree placement:
#' `position` only decides **where the tree is drawn**, never where the labels
#' go.  The labels always stay on their usual side of the panel, so the tree can
#' sit on the opposite side without moving anything the reader is used to.
#'
#' * `"bottom"` / `"top"` (x axis): below or above the labels.  With facets the
#'   tree is moved into the strip band, i.e. directly **under the strip** and
#'   above the panel, so strip, tree, labels and panel never overlap.  Use
#'   `strip_side = "above"` to draw it outside the strip instead; either way a
#'   small gap (`options(ggutils.tree_gap = 0.14)`, in centimetres) keeps the
#'   branches from touching the strip.
#' * `"left"` / `"right"` (y axis): left of the labels, or at the far end of the
#'   panel row.  With facets, `"left"` draws one tree per *visible* y axis;
#'   `"right"` draws one at the end of every panel row.
#'
#' A tree that has to sit away from the labels cannot travel through the axis
#' guide (a discrete secondary axis cannot carry a free panel's categories), so
#' it is drawn straight into the finished gtable, where the panel geometry is
#' known.  That is also what makes `facet_grid()` work: the row facet owns the y
#' axis and the column facet owns the x axis, and each tree is attached to the
#' strip of its own cell.
#'
#' @section Facets and `scales`:
#' The `scales` argument of `facet_wrap()` / `facet_grid()` is read
#' automatically, so `free` never has to be written by hand:
#'
#' * `"fixed"`: one shared tree for every panel.
#' * `"free_x"`: each panel column gets its own x tree.
#' * `"free_y"`: y trees are recomputed per panel row.
#' * `"free"`: both axes are recomputed panel by panel.
#'
#' `facet_wrap()` draws one x tree per panel row (each under its own strip) and
#' one y tree per visible y axis; `facet_grid()` draws one x tree per panel and
#' one y tree per row.  Writing `free = FALSE` forces a single shared tree,
#' `free = TRUE` forces per-panel trees regardless of `scales`.
#'
#' @section Free facets:
#' `facet_wrap(scales = "free")` gives every panel its own set of categories,
#' and with `free = TRUE` its own tree.  A discrete scale's limits are global,
#' so the categories are re-keyed per panel internally: each panel's focal
#' column becomes `"<panel>__ggcl__<category>"` and is ordered by that panel's
#' own `hclust()` leaf order.  The axis therefore really is in leaf order, a
#' standard tree can be drawn without a crossing branch, and the prefix never
#' reaches the printed labels.  Users never see the keys.
#'
#' With an explicitly supplied matrix (`data = mat`) the axis stays in data
#' order and the panel tree falls back to merging only **adjacent** clusters,
#' which keeps the layout planar at the cost of a shape that differs from a
#' plain `hclust()`.
#'
#' @section Deriving the matrix from the plot:
#' `scale_x_cluster()` and `scale_y_cluster()` do not need a matrix.  When
#' `data` is omitted, the matrix is built from the plot data at draw time:
#'
#' 1. the focal axis (`x` for [scale_x_cluster()], `y` for [scale_y_cluster()])
#'    provides the rows, the other positional axis provides the columns;
#' 2. the values come from the first numeric aesthetic mapped among `fill`,
#'    `colour`, `size`, `linewidth`, `alpha` and `weight`; use the `value`
#'    argument to pick another column;
#' 3. with no numeric aesthetic, a continuous other axis is summarised per
#'    category (`mean`, `sd`, `median`, `n`), and two categorical axes are
#'    clustered on how often the categories co-occur (a message says so).
#'
#' ```r
#' ggplot(df, aes(gene, sample, fill = value)) +
#'   geom_tile() +
#'   scale_x_cluster()                       # no matrix needed
#' ```
#'
#' Missing focal/other combinations are filled with the row mean instead of
#' dropping the category, so an incomplete grid still keeps every label on the
#' axis.  `hclust` remains available for a pre-computed tree.
#'
#' A guide on its own cannot change the order in which a scale maps categories
#' to positions: that order is fixed before the guide is built.  Therefore, to
#' both draw the tree *and* re-order the axis, use `scale_x_cluster()` /
#' `scale_y_cluster()`.  If you attach `guide_axis_cluster()` directly with
#' `guides(x = ...)`, the tree is aligned to the categories but the data order
#' is left unchanged.
#'
#' @section Radial coordinates (`coord_radial()`):
#' On a circular plot the theta (x) and r (y) axes are handled differently.
#'
#' **theta axis (x) -- ring dendrogram.**  The category labels keep their stock
#' `guide_axis_theta()` look and the tree becomes a *ring dendrogram*: leaves
#' on a circle, every merge an arc at another radius, joined to its children by
#' radial stubs.  `type = "triangle"` draws straight chords to each merge
#' instead of arcs.
#'
#' * `position = "bottom"` (default) draws the tree **inside the hole**: the
#'   leaves sit on the inner circle and every merge steps towards the centre.
#'   This needs `inner.radius > 0`; a pie without a hole has no room, so the
#'   tree is skipped.
#' * `position = "top"` draws it **outside the circle**: leaves on the outer
#'   circle, merges growing outwards.  With the default `ring_gap = NULL` the
#'   leaf ring starts after the longest theta label, and extra padding is
#'   inserted around every panel, so facet strips and neighbouring panels are
#'   never touched.  Leave some `plot.margin` / device room for the tree.
#'
#' **r axis (y) -- partial arcs only.**  On a full circle the r axis is a plain
#' column, so `scale_y_cluster()` draws **no** tree there.  With a partial arc
#' (`start`/`end`) ggplot2 puts the r labels on a radial ray and the tree
#' becomes an **arc-shaped wedge dendrogram** at the other end of the arc, the
#' side without labels: the leaf position becomes a radius and the merge
#' height an angle, so every straight branch becomes a concentric arc and the
#' tree hugs the heat map.  `height` sets the angular width
#' (`min(pi/3, 0.30 * height)` radians).
#'
#' **Facets.**  Every panel gets its own tree, and with
#' `facet_wrap(scales = "free_x")` that tree is clustered from the panel's own
#' genes, so the panels differ.  With fixed scales one shared tree is used.
#'
#' ```r
#' # per-sample z-scores: standardise every sample across the genes once
#' z <- local({
#'   m <- tapply(airway_heatmap$df$z,
#'               list(airway_heatmap$df$gene, airway_heatmap$df$sample), mean)
#'   m <- scale(m)                                # column-wise = per sample
#'   out <- as.data.frame(as.table(m))
#'   names(out) <- c("gene", "sample", "z_sample")
#'   merge(out, airway_heatmap$genes[c("gene", "gene_set")], by = "gene")
#' })
#' d <- droplevels(z[z$gene %in% unique(z$gene)[1:16], ])
#' fill_scale <- scale_fill_gradient2(low = "#2166ac", mid = "white",
#'                                    high = "#b2182b", midpoint = 0,
#'                                    name = "z (sample)")
#'
#' # (1) x tree inside the hole: bottom + inner.radius
#' ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
#'   coord_radial(theta = "x", inner.radius = 0.55) +
#'   scale_x_cluster(method = "ward.D2", height = 1.2) +
#'   fill_scale
#'
#' # (2) x tree outside the circle: position = "top"
#' ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
#'   coord_radial(theta = "x") +
#'   scale_x_cluster(method = "ward.D2", height = 1, position = "top") +
#'   fill_scale
#'
#' # (3) partial arc + r-axis tree at the label-free end of the arc
#' ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
#'   coord_radial(theta = "x", start = -0.5 * pi, end = 0.5 * pi,
#'                inner.radius = 0.55) +
#'   scale_y_cluster(method = "ward.D2", height = 1.6) +
#'   fill_scale
#'
#' # (4) facetted: one tree per panel, clustered per panel
#' ggplot(d, aes(gene, sample, fill = z_sample)) + geom_tile() +
#'   facet_wrap(~ gene_set, scales = "free_x") +
#'   coord_radial(theta = "x", inner.radius = 0.45) +
#'   scale_x_cluster(method = "ward.D2", height = 0.8) +
#'   fill_scale +
#'   theme_grey(base_size = 7)
#' ```
#'
#' @section Faceted free scales:
#' With `facet_wrap(scales = "free_x")` (or `"free_y"`) every panel has its own
#' categories, so one pre-computed tree cannot fit all panels.  In that case
#' pass the clustering data to the guide itself and set `free = TRUE`:
#'
#' ```r
#' ggplot(d, aes(gene, value)) +
#'   geom_point() +
#'   facet_wrap(~ cluster, scales = "free_x") +
#'   guides(x = guide_axis_cluster(data = mat, free = TRUE, method = "average"))
#' ```
#'
#' The guide then takes, in each panel, the rows of `mat` whose names match
#' that panel's categories, clusters that subset, and draws a tree sized for
#' exactly those leaves.  When the matrix was derived from the plot data (no
#' `data` argument), the per-panel matrix is rebuilt from that long data, so
#' combinations missing from a panel cannot become `NA` and drop a category.
#' The scale must leave the category order to the data (do **not** set global
#' `limits`); if you also want the categories re-ordered per panel, use
#' `scale_x_cluster()`/`scale_y_cluster()` with `free = TRUE`, which re-orders
#' each panel's axis into that panel's leaf order for you.
#'
#' @section Canvas height:
#' The tree row is a **fixed** `height` (in cm), while panel rows are `1null`.
#' When the fixed rows (trees + rotated axis labels + strips) need more room than
#' the device provides, gtable gives the null panel rows zero height and the
#' figure looks like trees and axis text only.  Leave roughly 2-3 cm of height
#' per panel row -- with two rows of faceted panels plus rotated labels, a
#' `fig.height` of 4 inches is already too short.  Either enlarge the device or
#' lower `height`.
#'
#' @return `guide_axis_cluster()` returns a guide object;
#'   `scale_x_cluster()` / `scale_y_cluster()` return a discrete ggplot2 scale.
#'
#' @seealso [ggplot2::guide_axis()], [ggplot2::scale_x_discrete()]
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' set.seed(1)
#' mat <- matrix(rnorm(12 * 5), nrow = 12,
#'               dimnames = list(paste0("g", 1:12), paste0("s", 1:5)))
#' long <- as.data.frame(as.table(mat))
#' names(long) <- c("gene", "sample", "value")
#'
#' # x axis: genes are clustered and re-ordered, tree above the labels
#' ggplot(long, aes(gene, sample, fill = value)) +
#'   geom_tile() +
#'   scale_x_cluster(mat, method = "average", height = 1.2, col = "#b2182b") +
#'   scale_y_discrete(guide = guide_axis(show.tick.label = TRUE))
#'
#' # y axis: same matrix, tree to the right of the labels
#' ggplot(long, aes(sample, gene, fill = value)) +
#'   geom_tile() +
#'   scale_y_cluster(mat, method = "ward.D2", height = 1.2, col = "#2166ac")
#'
#' # No matrix at all: the tree is clustered from the plot data itself
#' ggplot(long, aes(gene, sample, fill = value)) +
#'   geom_tile() +
#'   scale_x_cluster(method = "average", position = "top")
#'
#' # Tree away from the labels: x labels stay at the bottom, the tree moves
#' # under the strip; y labels stay on the left, the tree moves to the right
#' ggplot(long, aes(gene, sample, fill = value)) +
#'   geom_point() +
#'   facet_wrap(~ sample, scales = "free_x") +
#'   scale_x_cluster(position = "top", height = 1)
#'
#' ggplot(long, aes(gene, sample, fill = value)) +
#'   geom_point() +
#'   facet_wrap(~ sample, scales = "free_y") +
#'   scale_y_cluster(position = "left", height = 1)
#'
#' # Both axes at once; `scales` is picked up from the facet
#' ggplot(long, aes(gene, sample, fill = value)) +
#'   geom_point() +
#'   facet_wrap(~ sample, scales = "free") +
#'   scale_x_cluster(position = "top", height = 1) +
#'   scale_y_cluster(position = "left", height = 1)
#'
#' # facet_grid: the row facet owns y, the column facet owns x.
#' # Group both axes so the grid stays small; drop = FALSE keeps the empty
#' # levels visible, which is what makes "fixed" and "free_y" differ.
#' grid_df <- long
#' grid_df$g_half <- factor(ifelse(grid_df$gene %in% paste0("g", 1:6),
#'                                 "g1-g6", "g7-g12"))
#' grid_df$s_half <- factor(ifelse(grid_df$sample %in% c("s1", "s2"),
#'                                 "s1-s2", "s3-s5"))
#' ggplot(grid_df, aes(gene, sample, fill = value)) +
#'   geom_point() +
#'   facet_grid(s_half ~ g_half, scales = "free", drop = FALSE) +
#'   scale_x_cluster(position = "top", height = 1) +
#'   scale_y_cluster(position = "left", height = 1)
#' }
#'
#' @export
guide_axis_cluster <- function(data = NULL,
                               method = c("ward.D", "ward.D2", "single",
                                          "complete", "average", "mcquitty",
                                          "median", "centroid"),
                               height = 1,
                               col = "black",
                               lwd = 0.7,
                               type = c("rectangle", "triangle"),
                               hclust = NULL,
                               free = NULL,
                               map_data = NULL,
                               .tree_labels = NULL,
                               cluster_position = NULL,
                               show_labels = TRUE,
                               draw_tree = TRUE,
                               title = ggplot2::waiver(),
                               position = ggplot2::waiver(),
                               order = 0,
                               theme = NULL,
                               ...) {
  method <- match.arg(method)
  type <- match.arg(type)
  if (!is.null(free) && !is.logical(free)) {
    cli::cli_abort("{.arg free} must be {.code TRUE}, {.code FALSE} or {.code NULL}.")
  }
  if (isTRUE(free) && is.null(data)) {
    cli::cli_abort("{.arg free = TRUE} needs {.arg data}: a matrix whose rows are named by the categories.")
  }
  if (!is.null(hclust) && !inherits(hclust, "hclust")) {
    cli::cli_abort("{.arg hclust} must be an {.cls hclust} object.")
  }
  if (is.null(hclust) && is.null(data)) {
    cli::cli_abort(c(
      "Supply either {.arg data} or {.arg hclust}.",
      i = "For a tree clustered from the plot data, use {.fn scale_x_cluster} or {.fn scale_y_cluster} without {.arg data}."
    ))
  }
  if (!.ggcl_is_number(height) || height <= 0) {
    cli::cli_abort("{.arg height} must be a single positive number.")
  }
  if (!.ggcl_is_number(lwd) || lwd <= 0) {
    cli::cli_abort("{.arg lwd} must be a single positive number.")
  }

  info <- if (!is.null(hclust)) {
    .ggcl_from_hclust(hclust)
  } else {
    .ggcl_prepare(data, method)
  }

  ggplot2::new_guide(
    title    = title,
    position = position,
    order    = order,
    theme    = theme,
    hclust   = info$hclust,
    labels   = info$labels,
    data_mat = if (!is.null(data)) .ggcl_as_matrix(data) else NULL,
    map_data = map_data,
    tree_labels = .tree_labels,
    cluster_position = cluster_position,
    method   = method,
    free     = free,
    show_labels = isTRUE(show_labels),
    draw_tree = isTRUE(draw_tree),
    height   = as.numeric(height),
    tree_col = as.character(col)[1L],
    lwd      = as.numeric(lwd),
    type     = type,
    name     = "axis",
    available_aes = c("x", "y", "theta"),
    super    = GuideAxisCluster,
    ...
  )
}

#' @rdname guide_axis_cluster
#' @format NULL
#' @usage NULL
#' @export
GuideAxisCluster <- ggplot2::ggproto(
  "GuideAxisCluster",
  ggplot2::GuideAxis,
  params = c(
    ggplot2::GuideAxis$params,
    list(hclust = NULL, labels = NULL, data_mat = NULL, map_data = NULL,
         tree_labels = NULL, cluster_position = NULL, method = "complete",
         free = NULL, show_labels = TRUE, draw_tree = TRUE, height = 1,
         tree_col = "black",
         lwd = 0.7, type = "rectangle")
  ),

  ## -- coord_radial(): theta axis ------------------------------------------
  ## `coord_radial()` renames the position scales to `theta` / `r` and only
  ## keeps guides that know the `theta` aesthetic.  On a theta axis the labels
  ## are drawn exactly like `guide_axis_theta()` (everything below delegates to
  ## ggplot2's internal `GuideAxisTheta`); the dendrogram is not an axis-slot
  ## tree, it is a ring dendrogram drawn into the finished gtable by
  ## `.ggcl_ring_trees_to_gtable()`.
  transform = function(self, params, coord, panel_params) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$transform(params, coord, panel_params))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$transform(
      params, coord, panel_params
    )
  },

  setup_params = function(self, params) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$setup_params(params))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$setup_params(params)
  },

  override_elements = function(self, params, elements, theme) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$override_elements(params, elements, theme))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$override_elements(
      params, elements, theme
    )
  },

  build_decor = function(self, decor, grobs, elements, params) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$build_decor(decor, grobs, elements, params))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$build_decor(
      decor, grobs, elements, params
    )
  },

  arrange_layout = function(self, key, sizes, params, elements) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$arrange_layout(key, sizes, params, elements))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$arrange_layout(
      key, sizes, params, elements
    )
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$assemble_drawing(
        grobs, layout, sizes, params, elements
      ))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$assemble_drawing(
      grobs, layout, sizes, params, elements
    )
  },

  draw_early_exit = function(self, params, elements) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$draw_early_exit(self, params, elements))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$draw_early_exit(
      params, elements
    )
  },

  build_ticks = function(self, key, elements, params,
                         position = params$opposite) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$build_ticks(
        key, elements, params, params$position
      ))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$build_ticks(
      key, elements, params, position
    )
  },

  ## `show_labels = FALSE` keeps only the tree, for a secondary axis that
  ## carries the dendrogram while the main axis keeps the category names.  The
  ## labels are made invisible instead of removed, so the axis layout (and the
  ## grob/measure bookkeeping) stays untouched.
  setup_elements = function(self, params, elements, theme) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$setup_elements(params, elements, theme))
    }
    elements <- ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$setup_elements(
      params, elements, theme
    )
    if (isFALSE(params$show_labels)) {
      elements$text <- ggplot2::element_text(
        size = 0, colour = NA, hjust = 0.5, vjust = 0.5
      )
    }
    elements
  },

  build_labels = function(self, key, elements, params) {
    if (.ggse_is_theta(params)) {
      if (".label" %in% names(key)) key$.label <- .ggcl_strip_key(key$.label)
      return(.ggse_guide_theta()$build_labels(key, elements, params))
    }
    ## The tree needs the raw key: with free facets the break values carry a
    ## "<panel>__ggcl__" prefix that keeps each panel's categories separate, and
    ## the per-panel re-clustering matches on those full values.
    tree <- if (isFALSE(params$draw_tree)) NULL else .ggcl_tree_from_key(key, params)
    ## ...but the printed text must not show it.  Rewriting the key here (after
    ## the tree was matched) means the label grobs are built from the bare
    ## category names, so `measure_grobs()` reserves the right amount of space
    ## and the tree ends up next to the text instead of one prefix too far away.
    if (".label" %in% names(key)) {
      key$.label <- .ggcl_strip_key(key$.label)
    }
    labels <- ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$build_labels(
      key, elements, params
    )
    ## Always append a slot: `measure_grobs()` and the axis layout assume the
    ## tree slot exists, and a placeholder keeps them in sync when no tree can
    ## be drawn (fewer than two categories, unmatched labels, ...).
    c(labels, list(tree %||% grid::nullGrob()))
  },

  ## Reserve one extra row/column for the tree.  The stock implementation
  ## measures each label grob separately (two grobs here: labels + tree), which
  ## would create an extra gtable slot, so the four axis sizes are computed
  ## explicitly with the total label size in one slot, as in guide_axis_smart().
  measure_grobs = function(self, grobs, params, elements) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$measure_grobs(grobs, params, elements))
    }
    major_cm <- grid::convertUnit(elements$major_length, "cm", valueOnly = TRUE)
    rng <- range(0, major_cm)
    if (isTRUE(params$minor.ticks) &&
        !ggplot2::is_theme_element(elements$minor, "blank")) {
      minor_cm <- grid::convertUnit(elements$minor_length, "cm", valueOnly = TRUE)
      rng <- range(rng, minor_cm)
    }
    tick_size <- grid::unit(rng[2], "cm")
    spacer <- max(grid::unit(0, "pt"), grid::unit(-1 * diff(rng), "cm"))
    vertical <- isTRUE(params$vertical)
    ## `grobs$labels` holds the label grobs *plus* the tree appended by
    ## `build_labels()`.  The tree carries `ggse_size_cm = height`, so summing
    ## over the whole list would charge the tree's size to the labels as well
    ## and leave an extra `height` of blank space between the panel and the
    ## tree.  Only real labels count here.
    lab_grobs <- grobs$labels
    is_tree <- vapply(
      lab_grobs, inherits, logical(1), what = "ggutils_cluster_tree"
    )
    lab_cm <- vapply(lab_grobs[!is_tree], function(g) {
      s <- attr(g, "ggse_size_cm")
      if (is.null(s) || !is.finite(s)) {
        .ggse_grob_size_cm(g, vertical)
      } else {
        s
      }
    }, numeric(1))
    ## A y axis hands each label its own grob, laid out side by side across the
    ## axis: adding their widths would reserve one column per category and push
    ## the tree far away from the text.  Only the widest one matters.  A
    ## horizontal axis may stack label rows, so those heights do add up.
    labels_cm <- if (isTRUE(vertical)) {
      if (length(lab_cm)) max(lab_cm) else 0
    } else {
      sum(lab_cm)
    }
    title_cm <- .ggse_grob_size_cm(grobs$title, vertical)
    sizes <- grid::unit.c(
      tick_size, spacer, grid::unit(labels_cm, "cm"), grid::unit(title_cm, "cm")
    )
    if (isTRUE(params$lab_first)) sizes <- rev(sizes)
    ## `grobs$labels` is the list of label grobs with the tree appended last.
    lab <- grobs$labels
    has_tree <- length(lab) > 0L &&
      inherits(lab[[length(lab)]], "ggutils_cluster_tree")
    tree_size <- grid::unit(
      if (has_tree) params$height %||% 1 else 0, "cm"
    )
    if (isTRUE(params$lab_first)) {
      ## Slots run title -> labels -> spacer -> tick (outside in).  The tree has
      ## to sit between the title and the labels: `arrange_layout()` maps the
      ## grobs (ticks, labels..., title) onto the slots in reverse, so putting
      ## the tree first would hand it the (zero-width) title slot and let it
      ## overflow into the labels.
      return(grid::unit.c(sizes[1], tree_size, sizes[2:4]))
    }
    grid::unit.c(sizes[1:3], tree_size, sizes[4])
  }
)

## ---------------------------------------------------------------------------
## Clustering helpers
## ---------------------------------------------------------------------------

.ggcl_is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x)
}

.ggcl_methods <- c("ward.D", "ward.D2", "single", "complete", "average",
                   "mcquitty", "median", "centroid")

.ggcl_as_matrix <- function(data) {
  if (inherits(data, "hclust")) {
    cli::cli_abort("Use {.arg hclust} instead of {.arg data} for an {.cls hclust} object.")
  }
  if (is.data.frame(data) || is.matrix(data)) {
    mat <- as.matrix(data)
  } else {
    cli::cli_abort(
      "{.arg data} must be a numeric matrix or data frame with one row per category."
    )
  }
  if (!is.numeric(mat)) {
    cli::cli_abort("{.arg data} must contain numeric values only.")
  }
  if (ncol(mat) < 1L || nrow(mat) < 2L) {
    cli::cli_abort("{.arg data} needs at least two rows and one column.")
  }
  if (is.null(rownames(mat))) {
    rownames(mat) <- as.character(seq_len(nrow(mat)))
  }
  mat
}

.ggcl_from_hclust <- function(hc) {
  labels <- hc$labels
  if (is.null(labels)) {
    labels <- as.character(seq_along(hc$order))
  }
  list(hclust = hc, labels = as.character(labels))
}

.ggcl_prepare <- function(data, method = "complete") {
  method <- match.arg(method, .ggcl_methods)
  mat <- .ggcl_as_matrix(data)
  keep <- stats::complete.cases(mat)
  if (!all(keep)) {
    cli::cli_warn(c(
      "{sum(!keep)} row{?s} with missing values were removed before clustering."
    ))
    mat <- mat[keep, , drop = FALSE]
  }
  if (nrow(mat) < 2L) {
    cli::cli_abort("Fewer than two complete rows remain; cannot cluster.")
  }
  hc <- stats::hclust(stats::dist(mat), method = method)
  list(hclust = hc, labels = rownames(mat))
}

#' Retrieve the dendrogram leaf order
#'
#' @inheritParams guide_axis_cluster
#' @return A character vector of labels in dendrogram leaf order.
#' @export
cluster_order <- function(data, method = "complete") {
  info <- .ggcl_prepare(data, method)
  info$labels[info$hclust$order]
}

## ---------------------------------------------------------------------------
## Companion scales: re-order categories, then attach the tree
## ---------------------------------------------------------------------------

#' @rdname guide_axis_cluster
#' @export
scale_x_cluster <- function(data = NULL,
                            method = "complete",
                            height = 1,
                            ring_gap = NULL,
                            col = "black",
                            lwd = 0.7,
                            type = c("rectangle", "triangle"),
                            free = NULL,
                            breaks = NULL,
                            labels = ggplot2::waiver(),
                            limits = NULL,
                            expand = ggplot2::waiver(),
                            position = "bottom",
                            tree_position = NULL,
                            strip_side = c("below", "above"),
                            name = ggplot2::waiver(),
                            guide = NULL,
                            value = NULL,
                            .map_data = NULL,
                            ...) {
  if (is.null(data)) {
    ## The matrix is derived from the plot data, which is only available when
    ## the scale is added to a plot.
    return(.ggcl_defer("x", list(
      method = method, height = height, col = col, lwd = lwd, type = type,
      free = free, breaks = breaks, labels = labels, limits = limits,
      expand = expand, position = position, tree_position = tree_position,
      strip_side = strip_side, ring_gap = ring_gap,
      name = name, guide = guide, value = value, dots = list(...)
    )))
  }
  type <- match.arg(type)
  strip_side <- match.arg(strip_side)
  ## `position` (and its alias `tree_position`) says where the tree goes; the
  ## category names always stay on the bottom x axis, so `position = "top"`
  ## simply adds a tree above the panel and leaves the labels where they were.
  tree_pos <- tree_position %||% position
  if (!tree_pos %in% c("top", "bottom")) {
    cli::cli_abort("{.arg position} must be {.val top} or {.val bottom} for an x axis.")
  }
  info <- .ggcl_prepare(data, method)
  order_labels <- info$labels[info$hclust$order]
  ## `position` only says where the tree goes.  When it is on the same side as
  ## the category names (`bottom`), the guide draws both in one axis slot.  When
  ## it is not (`top`), the tree is injected into the gtable later
  ## (`.ggcl_inject_trees()`): a discrete secondary axis cannot reproduce a
  ## `facet_wrap(scales = "free")` panel's own categories, so it cannot carry
  ## the labels either, and the bottom axis keeps them.
  axis_side <- "bottom"
  ## The clustering travels with the plot so the finished gtable can draw it:
  ## on a cartesian axis a tree that left the axis slot is injected above the
  ## panel, and on a `coord_radial()` theta axis it becomes a ring dendrogram.
  inject <- list(
    axis = "x", position = tree_pos, height = height, col = col, lwd = lwd,
    type = type, free = free, method = method, strip_side = strip_side,
    hclust = info$hclust, panel_hclust = NULL,
    mat = tryCatch(.ggcl_as_matrix(data), error = function(e) NULL),
    ring_gap = ring_gap,
    draw_in_guide = identical(tree_pos, "bottom")
  )
  if (is.null(guide)) {
    ## Always use the cluster guide: on a `coord_radial()` theta axis a plain
    ## `guide_axis()` would drop the category labels, and the cluster guide
    ## delegates to the theta axis there.  With `position = "top"` it only
    ## draws the labels here; the tree itself is injected above the panel.
    guide <- guide_axis_cluster(
      data = data, hclust = info$hclust, height = height, col = col,
      lwd = lwd, type = type, free = free, position = "bottom",
      map_data = .map_data, .tree_labels = info$labels,
      cluster_position = tree_pos,
      draw_tree = identical(tree_pos, "bottom")
    )
  }
  out <- ggplot2::scale_x_discrete(
    name = name,
    breaks = if (isTRUE(free)) breaks %||% ggplot2::waiver() else breaks %||% order_labels,
    labels = labels,
    ## In per-panel (free) mode an explicit `limits` would force every panel
    ## to show every category; let the facets train their own limits.
    limits = if (isTRUE(free)) limits else limits %||% order_labels,
    expand = expand,
    guide = guide,
    position = axis_side,
    ...
  )
  ## Marks the plot so `.ggcl_trees_to_strip()` can move the tree inside the
  ## facet strip once the gtable is assembled.
  class(out) <- c("ggutils_cluster_scale", class(out))
  attr(out, "ggcl_spec") <- inject
  out
}

#' @rdname guide_axis_cluster
#' @export
scale_y_cluster <- function(data = NULL,
                            method = "complete",
                            height = 1,
                            col = "black",
                            lwd = 0.7,
                            type = c("rectangle", "triangle"),
                            free = NULL,
                            breaks = NULL,
                            labels = ggplot2::waiver(),
                            limits = NULL,
                            expand = ggplot2::waiver(),
                            position = "left",
                            tree_position = NULL,
                            strip_side = c("below", "above"),
                            name = ggplot2::waiver(),
                            guide = NULL,
                            value = NULL,
                            .map_data = NULL,
                            ...) {
  if (is.null(data)) {
    return(.ggcl_defer("y", list(
      method = method, height = height, col = col, lwd = lwd, type = type,
      free = free, breaks = breaks, labels = labels, limits = limits,
      expand = expand, position = position, tree_position = tree_position,
      strip_side = strip_side,
      name = name, guide = guide, value = value, dots = list(...)
    )))
  }
  type <- match.arg(type)
  strip_side <- match.arg(strip_side)
  ## As for x: `position` places the tree, while the category names always keep
  ## the habitual left y axis.
  tree_pos <- tree_position %||% position
  if (!tree_pos %in% c("left", "right")) {
    cli::cli_abort("{.arg position} must be {.val left} or {.val right} for a y axis.")
  }
  info <- .ggcl_prepare(data, method)
  order_labels <- info$labels[info$hclust$order]
  ## Symmetric to the x scale: `left` draws tree and labels in the same axis
  ## slot, `right` only draws the labels and lets `.ggcl_inject_trees()` put the
  ## tree to the right of the panels.
  axis_side <- "left"
  inject <- list(
    axis = "y", position = tree_pos, height = height, col = col, lwd = lwd,
    type = type, free = free, method = method, strip_side = strip_side,
    hclust = info$hclust, panel_hclust = NULL,
    mat = tryCatch(.ggcl_as_matrix(data), error = function(e) NULL),
    draw_in_guide = FALSE
  )
  if (is.null(guide)) {
    ## A y axis can show different categories in different panels (`free_y`),
    ## and the axis guide then lays out its slots per panel; the tree ends up
    ## rotated in the panels that are not the first.  Drawing it in the gtable
    ## instead keeps every tree in its panel's own orientation.
    guide <- ggplot2::guide_axis()
  }
  out <- ggplot2::scale_y_discrete(
    name = name,
    breaks = if (isTRUE(free)) breaks %||% ggplot2::waiver() else breaks %||% order_labels,
    labels = labels,
    limits = if (isTRUE(free)) limits else limits %||% order_labels,
    expand = expand,
    guide = guide,
    position = axis_side,
    ...
  )
  class(out) <- c("ggutils_cluster_scale", class(out))
  attr(out, "ggcl_spec") <- inject
  out
}

## ---------------------------------------------------------------------------
## Dendrogram grob
## ---------------------------------------------------------------------------

.ggcl_axis_geometry <- function(position) {
  switch(position,
    bottom = list(near = 1, outward = -1, vertical = FALSE),
    top    = list(near = 0, outward =  1, vertical = FALSE),
    left   = list(near = 1, outward = -1, vertical = TRUE),
    right  = list(near = 0, outward =  1, vertical = TRUE),
    list(near = 1, outward = -1, vertical = FALSE)
  )
}

## Trees are drawn into a slot `height` cm thick.  Leaving a small gap at the far
## end (the side facing the strip) keeps the branches clear of the strip while
## the leaves stay flush with the panel edge.
.ggcl_tree_gap <- function(params = NULL) {
  gap <- params$tree_gap %||% getOption("ggutils.tree_gap", 0.14)
  max(0, as.numeric(gap))
}

## Map a depth value in [0, 1] (0 = panel edge, 1 = far end) to centimetres,
## shrinking the span by `gap` at the far end.
.ggcl_depth_unit <- function(v, height, geom, gap) {
  gap <- min(gap, height / 3)
  span <- height - gap
  off <- if (identical(as.numeric(geom$near), 0)) 0 else gap
  grid::unit(off + v * span, "cm")
}

.ggcl_tree_labels <- function(params) {
  tl <- params$tree_labels
  if (!is.null(tl) && is.character(tl)) return(as.character(tl))
  lb <- params$labels
  if (is.character(lb)) return(as.character(lb))
  character(0)
}
.ggcl_tree_from_key <- function(key, params) {
  major <- key$.type %||% "major"
  key <- key[major == "major", , drop = FALSE]
  aes_name <- params$aes %||% "x"
  if (!aes_name %in% names(key)) {
    aes_name <- if ("x" %in% names(key)) "x" else "y"
  }
  pos <- as.numeric(key[[aes_name]])
  labels <- as.character(key$.label)
  if (length(pos) == 0L || length(labels) == 0L) return(NULL)

  ## Category names used for matching the tree's leaves.  `params$labels` is a
  ## *display* value and may be a function (free facets strip a panel prefix),
  ## so the scale passes the raw names separately.
  all_labels <- .ggcl_tree_labels(params)
  if (length(all_labels) &&
      !all(labels %in% all_labels) &&
      length(labels) == length(all_labels)) {
    labels <- all_labels
  }

  ## Facetted free scales: the key holds only this panel's categories.  Re-cluster
  ## the matching rows of data_mat so every panel gets its own tree and leaf
  ## order.  `free = NULL` (auto) does this only when the facets actually split
  ## the categories, so a fixed scale keeps a single shared tree.
  mat <- params$data_mat
  long <- params$map_data
  recluster <- isTRUE(params$free) ||
    (is.null(params$free) && (!is.null(mat) || !is.null(long)) &&
       !setequal(labels, as.character(params$labels)))
  if (recluster) {
    sub <- NULL
    if (!is.null(long) && nrow(long)) {
      ## Rebuild the panel matrix from the long frame, so combinations that do
      ## not exist in this panel cannot turn into NA and drop a category.
      part <- long[long$.focal %in% labels, , drop = FALSE]
      if (nrow(part)) {
        sub <- .ggcl_matrix_from_long(part)
        sub <- sub[intersect(labels, rownames(sub)), , drop = FALSE]
      }
    } else if (!is.null(mat)) {
      idx <- match(labels, rownames(mat))
      if (anyNA(idx)) return(NULL)
      sub <- mat[idx, , drop = FALSE]
    }
    if (is.null(sub) || nrow(sub) < 2L) return(NULL)
    ## The axis of a free panel is in that panel's leaf order (the focal column
    ## is re-keyed per panel), so a plain `hclust()` already yields a planar
    ## tree: its leaves line up with the ticks and no branch can cross.  With an
    ## explicitly supplied matrix the axis is in data order instead, and only a
    ## contiguity-constrained tree can stay planar there.
    method <- params$method %||% "complete"
    hc_std <- stats::hclust(stats::dist(sub), method = method)
    hc <- if (identical(rownames(sub)[hc_std$order], labels)) {
      hc_std
    } else {
      .ggcl_ordered_hclust(sub, method = method)
    }
    all_labels <- rownames(sub)
  } else {
    hc <- params$hclust
  }
  if (is.null(hc) || !inherits(hc, "hclust")) return(NULL)

  n <- length(hc$order)
  height <- params$height %||% 1
  tree_type <- params$type %||% "rectangle"

  ## Leaf positions follow the hclust leaf order.  When the companion scale was
  ## used the key is already in that order; otherwise match by label so the
  ## tree still sits over the right categories.
  ord_labels <- all_labels[hc$order]
  idx <- match(ord_labels, labels)
  if (anyNA(idx) || length(idx) != n) {
    idx <- seq_len(min(n, length(pos)))
  }
  leaf_pos <- pos[idx]
  if (length(leaf_pos) != n) return(NULL)

  ## `hc$merge` uses negative indices for the ORIGINAL observation number,
  ## while `leaf_pos` is ordered by `hc$order` (the dendrogram leaf order).
  ## Remap before indexing, otherwise the branches cross each other.
  leaf_at <- rep(NA_real_, n)
  leaf_at[hc$order] <- leaf_pos
  if (anyNA(leaf_at)) return(NULL)

  ## `params$position` is the *guide* position ggplot2 fills in, and it is not
  ## reliable across panels; the axis the tree belongs to decides the geometry.
  tree_pos <- params$cluster_position
  if (is.null(tree_pos)) {
    tree_pos <- if (identical(params$aes %||% "x", "y")) "left" else "bottom"
  }
  geom <- .ggcl_axis_geometry(tree_pos)
  node_pos <- numeric(max(0L, n - 1L))
  heights <- as.numeric(hc$height)
  hmax <- max(c(heights, 1e-8))
  h <- heights / hmax

  for (i in seq_len(n - 1L)) {
    a <- hc$merge[i, 1L]
    b <- hc$merge[i, 2L]
    xa <- if (a < 0L) leaf_at[-a] else node_pos[a]
    xb <- if (b < 0L) leaf_at[-b] else node_pos[b]
    node_pos[i] <- (xa + xb) / 2
  }

  ax <- numeric(0)
  ay <- numeric(0)
  bx <- numeric(0)
  by <- numeric(0)
  across_leaf <- geom$near
  for (i in seq_len(n - 1L)) {
    a <- hc$merge[i, 1L]
    b <- hc$merge[i, 2L]
    xa <- if (a < 0L) leaf_at[-a] else node_pos[a]
    xb <- if (b < 0L) leaf_at[-b] else node_pos[b]
    ha <- if (a < 0L) across_leaf else geom$near + geom$outward * h[a]
    hb <- if (b < 0L) across_leaf else geom$near + geom$outward * h[b]
    hp <- geom$near + geom$outward * h[i]

    if (identical(tree_type, "triangle")) {
      ## Two diagonals from the parent node to each child, as in
      ## plot.dendrogram(type = "triangle").
      if (isTRUE(geom$vertical)) {
        ax <- c(ax, ha, hb)
        ay <- c(ay, xa, xb)
        bx <- c(bx, hp, hp)
        by <- c(by, node_pos[i], node_pos[i])
      } else {
        ax <- c(ax, xa, xb)
        ay <- c(ay, ha, hb)
        bx <- c(bx, node_pos[i], node_pos[i])
        by <- c(by, hp, hp)
      }
    } else if (isTRUE(geom$vertical)) {
      ax <- c(ax, ha, hb, hp)
      ay <- c(ay, xa, xb, xa)
      bx <- c(bx, hp, hp, hp)
      by <- c(by, xa, xb, xb)
    } else {
      ax <- c(ax, xa, xb, xa)
      ay <- c(ay, ha, hb, hp)
      bx <- c(bx, xa, xb, xb)
      by <- c(by, hp, hp, hp)
    }
  }

  gap <- .ggcl_tree_gap(params)
  segments <- grid::segmentsGrob(
    x0 = if (isTRUE(geom$vertical)) .ggcl_depth_unit(ax, height, geom, gap) else grid::unit(ax, "npc"),
    y0 = if (isTRUE(geom$vertical)) grid::unit(ay, "npc") else .ggcl_depth_unit(ay, height, geom, gap),
    x1 = if (isTRUE(geom$vertical)) .ggcl_depth_unit(bx, height, geom, gap) else grid::unit(bx, "npc"),
    y1 = if (isTRUE(geom$vertical)) grid::unit(by, "npc") else .ggcl_depth_unit(by, height, geom, gap),
    gp = grid::gpar(
      col = params$tree_col %||% "black",
      lwd = params$lwd %||% 0.7,
      lineend = "round"
    )
  )
  .ggcl_tree_grob(segments, params$height %||% 1)
}

.ggcl_tree_grob <- function(segments, height_cm) {
  g <- grid::gTree(
    segments = segments,
    name = "ggutils.cluster.tree",
    cl = "ggutils_cluster_tree"
  )
  attr(g, "ggse_size_cm") <- height_cm
  g
}

makeContent.ggutils_cluster_tree <- function(x) {
  grid::setChildren(x, grid::gList(x$segments))
}
