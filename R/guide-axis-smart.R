#' Smart axis labels for ggplot2
#'
#' `guide_axis_smart()` is a drop-in replacement for [ggplot2::guide_axis()].
#' It is meant for discrete axes that carry more category labels than the panel
#' can display at once.  Instead of letting the labels overlap, it
#'
#' 1. displays only the labels selected with `which` (and/or at most `n`
#'    evenly spaced labels),
#' 2. distributes the displayed labels over `nrow` parallel tracks (rows for an
#'    x axis, columns for a y axis),
#' 3. connects every displayed label to its tick with a leader line, and
#' 4. reserves exactly as much axis space as the label stack needs, so the
#'    available page space is used efficiently.
#'
#' The one-dimensional arrangement of the labels inside each track is a port of
#' the `smartAlign` algorithm from the
#' [circlize](https://github.com/jokergoo/circlize) package.
#'
#' @param title A character string or `waiver()` for the axis title.
#' @param which Which labels should be displayed? `NULL` (the default) displays
#'   all labels that fit.  Otherwise one of
#'   * a numeric vector of positions (indices into the vector of breaks),
#'   * a character vector of label values,
#'   * a logical vector of the same length as the breaks,
#'   * a function of two arguments, `function(labels, positions)`, that returns
#'     a logical vector (or a numeric vector of indices).
#'   A zero-length vector (`integer(0)`, `character(0)`, `logical(0)`) selects
#'   no labels at all and draws an empty axis.
#'
#'   Facets are supported: the guide is built once per panel (or once per column
#'   for a shared axis), so a character `which` such as `c("gene1", "gene2")`
#'   only marks the gene in the panel where it actually appears.  Unmatched
#'   labels are silently skipped, which keeps a per-panel request quiet.
#' @param n Maximum number of labels to display.  When `which` is `NULL` and
#'   `n` is set, `n` evenly spaced labels are picked.  When `which` is given, at
#'   most `n` of the selected labels are kept.
#' @param nrow Number of parallel tracks used for the labels: rows for a
#'   horizontal axis, columns for a vertical axis.  `NULL` (the default) picks
#'   the number of tracks automatically: labels stay in one track whenever they
#'   fit, and are distributed over several tracks only when they crowd into a
#'   small region.  When a track count is supplied explicitly and a label does
#'   not fit, the label is dropped (with a message) instead of being drawn on
#'   top of another one.
#' @param padding Minimal gap between two label boxes in a track, as a fraction
#'   of the label height.  Defaults to `0.25`.  On a radial (`theta`) axis it is
#'   the gap between neighbouring labels measured along the arc.
#' @param gap Gap between two neighbouring tracks, as a fraction of the label
#'   height.  Defaults to `0.30`.
#' @param extend How far a label may be pushed beyond the axis range, as a
#'   fraction of the axis range.  The default `0` keeps every label inside the
#'   panel, so labels are never clipped at the edge of the device.  A positive
#'   value gives the layout more freedom but lets labels overhang the panel,
#'   which requires enough `plot.margin`.
#' @param connection Draw leader lines between ticks and labels?  Defaults to
#'   `TRUE`.
#' @param connection_style Shape of the leader line. `"diagonal"` (the default,
#'   the style used by `circlize` and `ComplexHeatmap::anno_mark()`) is a short
#'   segment perpendicular to the axis, then a diagonal, then another short
#'   perpendicular segment ending at the label. `"straight"` is a single line
#'   from the tick to the label and `"elbow"` is a perpendicular segment
#'   followed by a shelf parallel to the axis.
#' @param link_length Length in millimetres of the leader line between the axis
#'   and the first track of labels.  Defaults to `3`.
#' @param connection_col,connection_lwd,connection_lty Appearance of the leader
#'   lines.  `connection_col = NULL` uses the colour of the axis ticks.
#' @param tick Which ticks should be drawn: `"all"` (default), `"shown"` (only
#'   the ticks of the displayed labels) or `"none"`.
#' @param angle Rotation of the labels in degrees, `NULL` to inherit the theme.

#' @param theta_side Place the radial labels `"outside"` (the default) or
#'   `"inside"` the circle.  Inside labels sit on a label track inside the
#'   circle and grow towards the centre, which suits circular heatmaps with a
#'   large `inner.radius`.  Ignored on cartesian axes.
#' @param panel_width Width (for an x axis) or height (for a y axis) of the
#'   panel in centimetres.  Only used to estimate the number of tracks before
#'   the plot is rendered; `NULL` (the default) estimates it from the graphics
#'   device.  Set it when the automatic estimate produces too few tracks.
#' @param max_nrow Upper bound for the automatic number of tracks.  Defaults to
#'   `8`.
#' @param hjust,vjust Horizontal/vertical justification of a label inside its
#'   box.  `NULL` (the default) centres the text in the box, which keeps the
#'   leader lines aligned with the visual centre of the label.
#' @param minor.ticks `r lifecycle::badge("experimental")` Draw minor ticks,
#'   passed on to [ggplot2::guide_axis()].
#' @param cap,order,position,theme Passed on to [ggplot2::guide_axis()].
#'
#' @details
#' `coord_radial()` renames the position scales to `theta` and `r`, and only
#' accepts guides that know about the `theta` aesthetic; `guide_axis_smart()` is
#' one of them.  On a radial axis the labels are laid out the way
#' [circlize::circos.labels()] does it: the text is rotated along the radius so
#' that it points towards the centre of the circle (turned around on the left
#' half, so nothing is upside down) and every label is tied to its tick with a
#' three-segment leader line -- a short radial stub, a diagonal, then a short
#' radial stub at the label.
#'
#' `which` / `n` pick the categories that appear around the ring,
#' `tick = "shown"` only keeps their ticks, and `theta_side` chooses between a
#' label track `"outside"` the circle (the default) and one attached to the
#' `inner.radius()` of the circle (`"inside"`, useful for the hole of a circular
#' heatmap).  Labels that crowd into one stretch of the circle are spread along
#' the arc with the `smartAlign` algorithm (`connection = FALSE` removes the
#' leader lines), and a `start`/`end` arc (a half circle) is supported as well.
#' Every label stays anchored on its own tick and a crowded group is spread
#' symmetrically around that group, so the first label keeps sitting next to its
#' tick instead of the whole group sliding towards the end of the arc.
#'
#' The label size, colour, font family, face and line height always come from
#' the theme's axis text element (`axis.text.x` / `axis.text.y`), exactly like
#' the stock axis labels; `angle`, `hjust` and `vjust` override the theme only
#' when they are supplied.
#'
#' @return A `guide` object that can be used in `guides()` or in the `guide`
#'   argument of a scale.
#'
#' @seealso [smart_align()], [smart_stack()], [ggplot2::guide_axis()],
#'   [ggplot2::guide_axis_theta()]
#'
#' @examples
#' library(ggplot2)
#'
#' d <- data.frame(
#'   sample = factor(paste0("S", 1:40)),
#'   value  = rnorm(40)
#' )
#'
#' # 40 categories: labels are spread over several rows and connected to
#' # their ticks with leader lines
#' ggplot(d, aes(sample, value)) +
#'   geom_point() +
#'   scale_x_discrete(guide = guide_axis_smart())
#'
#' # only a hand-picked subset of labels is displayed
#' ggplot(d, aes(sample, value)) +
#'   geom_point() +
#'   scale_x_discrete(
#'     guide = guide_axis_smart(which = c("S1", "S10", "S20", "S30", "S40"))
#'   )
#'
#' # on a vertical axis the labels are stacked in columns
#' ggplot(d, aes(value, sample)) +
#'   geom_point() +
#'   scale_y_discrete(guide = guide_axis_smart(which = c("S1", "S20", "S40")))
#'
#' # `coord_radial()` (ggplot2 >= 3.5) names the position scales `theta` and `r`;
#' # attach the guide with `guides(theta = ...)` and it draws the selected labels
#' # along the ring, like `guide_axis_theta()`
#' ggplot(d, aes(sample, value)) +
#'   geom_col() +
#'   coord_radial(theta = "x") +
#'   guides(theta = guide_axis_smart(which = seq(1, 40, by = 4)))
#'
#' # the categories can also be the `theta` scale when they are mapped to y
#' ggplot(d, aes(value, sample)) +
#'   geom_col() +
#'   coord_radial(theta = "y") +
#'   guides(theta = guide_axis_smart(n = 10))
#'
#' # a crowded run of labels is spread along the arc and connected to its ticks
#' # with the three-segment leader lines
#' ggplot(d, aes(sample, value)) +
#'   geom_col() +
#'   coord_radial(theta = "x") +
#'   guides(theta = guide_axis_smart(which = 9:20))
#'
#' # only part of the circle (a half circle) works the same
#' ggplot(d, aes(sample, value)) +
#'   geom_col() +
#'   coord_radial(theta = "x", start = -0.5 * pi, end = 0.5 * pi) +
#'   guides(theta = guide_axis_smart(which = 9:20))
#'
#' # a circular heatmap from the bundled RNA-seq data (300 genes x 8 samples):
#' # the genes run around the circle, labels outside
#' data(airway_heatmap)
#' ggplot(airway_heatmap$df, aes(gene, sample, fill = z)) +
#'   geom_tile() +
#'   coord_radial(theta = "x", inner.radius = 0.2) +
#'   guides(theta = guide_axis_smart(which = airway_heatmap$mark)) +
#'   scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
#'                        midpoint = 0, name = "z-score")
#'
#' # the same heatmap with the labels attached to the inner radius (inside the
#' # hole); a large inner radius leaves room for them
#' ggplot(airway_heatmap$df, aes(gene, sample, fill = z)) +
#'   geom_tile() +
#'   coord_radial(theta = "x", inner.radius = 0.62) +
#'   guides(theta = guide_axis_smart(
#'     which = airway_heatmap$mark, theta_side = "inside"
#'   )) +
#'   scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
#'                        midpoint = 0, name = "z-score")
#'
#' # with facets every panel is a circle of its own
#' d_facet <- data.frame(
#'   panel = rep(c("A", "B"), each = 20),
#'   cat   = factor(rep(paste0("g", 1:20), 2), levels = paste0("g", 1:20)),
#'   value = rnorm(40)
#' )
#' ggplot(d_facet, aes(cat, value)) +
#'   geom_col(fill = "#3b82f6") +
#'   coord_radial(theta = "x") +
#'   guides(theta = guide_axis_smart(which = seq(1, 20, by = 2))) +
#'   facet_wrap(~panel)
#'
#' # a faceted circular heatmap needs scales = "free_x": each panel then has its
#' # own gene scale, so the ring is complete and only that panel's genes are
#' # labelled
#' ggplot(airway_heatmap$df, aes(gene, sample, fill = z)) +
#'   geom_tile() +
#'   coord_radial(theta = "x", inner.radius = 0.3) +
#'   guides(theta = guide_axis_smart(which = unlist(airway_heatmap$mark_by_set))) +
#'   facet_wrap(~gene_set, ncol = 2, scales = "free_x") +
#'   scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
#'                        midpoint = 0, name = "z-score")
#'
#' @export
guide_axis_smart <- function(title = ggplot2::waiver(),
                             which = NULL,
                             n = NULL,
                             nrow = NULL,
                             padding = 0.25,
                             gap = 0.30,
                             extend = 0,
                             connection = TRUE,
                             connection_style = c("diagonal", "straight", "elbow"),
                             link_length = 3,
                             connection_col = NULL,
                             connection_lwd = 0.5,
                             connection_lty = 1,
                             tick = c("all", "shown", "none"),
                             theta_side = c("outside", "inside"),
                             angle = NULL,
                             panel_width = NULL,
                             max_nrow = 8L,
                             hjust = NULL,
                             vjust = NULL,
                             minor.ticks = FALSE,
                             cap = "none",
                             order = 0,
                             position = ggplot2::waiver(),
                             theme = NULL) {
  connection_style <- match.arg(connection_style)
  tick <- match.arg(tick)
  theta_side <- match.arg(theta_side)

  check_which <- function(x) {
    if (is.null(x) || is.function(x) || is.logical(x) || is.numeric(x) ||
        is.character(x)) {
      return(invisible(NULL))
    }
    cli::cli_abort(c(
      "{.arg which} must be {.code NULL}, a numeric, character or logical vector, or a function.",
      "x" = "You supplied {.obj_type_friendly {which}}."
    ))
  }
  check_which(which)

  if (!is.null(n)) {
    n <- as.integer(n)
    if (length(n) != 1L || is.na(n) || n < 1L) {
      cli::cli_abort("{.arg n} must be a single positive integer or {.code NULL}.")
    }
  }
  if (!is.null(nrow)) {
    nrow <- as.integer(nrow)
    if (length(nrow) != 1L || is.na(nrow) || nrow < 1L) {
      cli::cli_abort("{.arg nrow} must be a single positive integer or {.code NULL}.")
    }
  }
  if (!.ggse_is_number(padding) || padding < 0) {
    cli::cli_abort("{.arg padding} must be a single non-negative number.")
  }
  if (!.ggse_is_number(gap) || gap < 0) {
    cli::cli_abort("{.arg gap} must be a single non-negative number.")
  }
  if (!.ggse_is_number(extend) || extend < 0 || extend > 0.5) {
    cli::cli_abort("{.arg extend} must be a single number between 0 and 0.5.")
  }
  if (!is.logical(connection) || length(connection) != 1L || is.na(connection)) {
    cli::cli_abort("{.arg connection} must be {.code TRUE} or {.code FALSE}.")
  }
  if (!.ggse_is_number(connection_lwd) || connection_lwd < 0) {
    cli::cli_abort("{.arg connection_lwd} must be a single non-negative number.")
  }
  if (!.ggse_is_number(link_length) || link_length < 0) {
    cli::cli_abort("{.arg link_length} must be a single non-negative number of millimetres.")
  }
  cap <- if (isTRUE(cap)) "both" else if (identical(cap, FALSE)) "none" else cap
  cap <- match.arg(cap, c("none", "both", "upper", "lower"))
  if (!is.null(panel_width)) {
    if (!.ggse_is_number(panel_width) || panel_width <= 0) {
      cli::cli_abort("{.arg panel_width} must be a single positive number or {.code NULL}.")
    }
  }
  max_nrow <- as.integer(max_nrow)
  if (length(max_nrow) != 1L || is.na(max_nrow) || max_nrow < 1L) {
    cli::cli_abort("{.arg max_nrow} must be a single positive integer.")
  }
  for (nm in c("hjust", "vjust")) {
    v <- get(nm)
    if (!is.null(v) && (!.ggse_is_number(v) || v < 0 || v > 1)) {
      cli::cli_abort("{.arg {nm}} must be {.code NULL} or a number between 0 and 1.")
    }
  }

  ggplot2::new_guide(
    title            = title,
    which            = which,
    n                = n,
    nrow             = nrow,
    padding          = padding,
    gap              = gap,
    extend           = extend,
    connection       = connection,
    connection_style = connection_style,
    link_length      = link_length,
    connection_col   = connection_col,
    connection_lwd   = connection_lwd,
    connection_lty   = connection_lty,
    tick             = tick,
    theta_side       = theta_side,
    angle            = angle,
    panel_width      = panel_width,
    max_nrow         = max_nrow,
    hjust            = hjust,
    vjust            = vjust,
    minor.ticks      = minor.ticks,
    cap              = cap,
    order            = order,
    position         = position,
    theme            = theme,
    available_aes    = c("x", "y", "theta"),
    name             = "axis",
    super            = GuideAxisSmart
  )
}

## Defaults for the extra parameters.  They are merged into the `params` field
## of the ggproto class below so that `new_guide()` accepts them.
.ggse_guide_params <- list(
  which            = NULL,
  n                = NULL,
  nrow             = NULL,
  padding          = 0.25,
  gap              = 0.30,
  extend           = 0,
  connection       = TRUE,
  connection_style = "diagonal",
  link_length      = 3,
  connection_col   = NULL,
  connection_lwd   = 0.5,
  connection_lty   = 1,
  tick             = "all",
  theta_side       = "outside",
  panel_width      = NULL,
  max_nrow         = 8L,
  hjust            = NULL,
  vjust            = NULL
)

## `coord_radial()` uses the `theta` / `theta.sec` guide positions.  Those are
## only known once the coordinate system has set up the guides, which is why the
## branch is decided from `params$position` inside the methods below.
.ggse_is_theta <- function(params) {
  position <- params$position
  !is.null(position) && length(position) == 1L && !is.na(position) &&
    position %in% c("theta", "theta.sec")
}

## ggplot2 >= 4.0 keeps `GuideAxisTheta` internal.  The radial branch reuses it
## so that the labels are placed and rotated exactly like `guide_axis_theta()`.
.ggse_guide_theta <- function() {
  ns <- asNamespace("ggplot2")
  if (!exists("GuideAxisTheta", envir = ns, inherits = FALSE)) {
    cli::cli_abort(c(
      "This version of ggplot2 does not provide `GuideAxisTheta`.",
      "i" = "`coord_radial()` support needs ggplot2 >= 4.0.0."
    ))
  }
  get("GuideAxisTheta", envir = ns, inherits = FALSE)
}

## Keep only the labels selected by `which` / `n` before the theta drawing is
## delegated to ggplot2.  `tick = "shown"` reuses the same selection for ticks.
.ggse_theta_filter_key <- function(key, params) {
  if (is.null(key) || nrow(key) == 0L) {
    return(key)
  }
  labels <- if (".label" %in% names(key)) {
    as.character(key$.label)
  } else {
    rep(NA_character_, nrow(key))
  }
  pos <- if ("theta" %in% names(key)) key$theta else NULL
  sel <- .ggse_selection(
    params$which %||% NULL, labels, pos = pos, n = params$n %||% NULL
  )
  sel[is.na(sel)] <- FALSE
  key[sel, , drop = FALSE]
}

#' @rdname guide_axis_smart
#' @format NULL
#' @usage NULL
#' @export
GuideAxisSmart <- ggplot2::ggproto(
  "GuideAxisSmart",
  ggplot2::GuideAxis,
  params = c(ggplot2::GuideAxis$params, .ggse_guide_params),

  ## -- coordinate system -----------------------------------------------------
  ## `coord_radial()` renames the position scales to `theta` / `r` and only
  ## keeps guides that know the `theta` aesthetic.  On a theta axis every
  ## drawing step is delegated to ggplot2's `GuideAxisTheta`, so the labels sit
  ## and rotate along the circle exactly like `guide_axis_theta()`; the smart
  ## part is the selection (`which` / `n`).  Tracks and leader lines are a
  ## cartesian feature and are ignored on a radial axis.
  transform = function(self, params, coord, panel_params) {
    if (.ggse_is_theta(params)) {
      params <- .ggse_guide_theta()$transform(params, coord, panel_params)
      ## `coord_radial(start =, end =)` may draw only part of the circle; the
      ## label layout needs the arc to normalise the angular positions.
      params$smart_arc <- panel_params$arc %||% c(0, 2 * pi)
      ## the inner radius of coord_radial() is needed to put the labels inside
      ## the ring (the circlize `side = "inside"` layout)
      params$smart_inner <- panel_params$inner_radius %||% 0
      return(params)
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

  setup_elements = function(self, params, elements, theme) {
    if (.ggse_is_theta(params)) {
      return(.ggse_guide_theta()$setup_elements(params, elements, theme))
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$setup_elements(
      params, elements, theme
    )
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

  ## -- ticks ---------------------------------------------------------------
  build_ticks = function(self, key, elements, params,
                         position = params$opposite) {
    if (.ggse_is_theta(params)) {
      tick_mode <- params$tick %||% "all"
      if (identical(tick_mode, "none")) {
        return(ggplot2::zeroGrob())
      }
      if (identical(tick_mode, "shown")) {
        key <- .ggse_theta_filter_key(key, params)
      }
      return(.ggse_guide_theta()$build_ticks(
        key, elements, params, params$position
      ))
    }
    tick_mode <- params$tick %||% "all"
    if (identical(tick_mode, "none")) {
      return(ggplot2::zeroGrob())
    }
    if (identical(tick_mode, "shown")) {
      prep <- .ggse_prepare(key, params, elements)
      if (is.null(prep) || !any(prep$shown)) {
        return(ggplot2::zeroGrob())
      }
      keep <- rep(FALSE, nrow(key))
      if (".type" %in% names(key)) {
        keep[which(key$.type == "major")] <- prep$shown
      } else {
        keep <- prep$shown
      }
      key <- key[keep, , drop = FALSE]
    }
    ggplot2::ggproto_parent(ggplot2::GuideAxis, self)$build_ticks(
      key, elements, params, position
    )
  },

  ## -- labels ---------------------------------------------------------------
  build_labels = function(self, key, elements, params) {
    if (.ggse_is_theta(params)) {
      key <- .ggse_theta_filter_key(key, params)
      return(.ggse_theta_label_grob(key, elements, params))
    }
    prep <- .ggse_prepare(key, params, elements)
    if (is.null(prep) || !any(prep$shown)) {
      return(list(ggplot2::zeroGrob()))
    }
    list(.ggse_label_grob(prep, params))
  },

  ## -- sizes ----------------------------------------------------------------
  ## The default implementation in ggplot2 4.0.x calls `measure()`, which does
  ## not exist any more, so the sizes are computed from scratch here.
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
    labels <- grid::unit(.ggse_labels_size_cm(grobs$labels), "cm")
    title <- grid::unit(.ggse_grob_size_cm(grobs$title, params$vertical), "cm")
    sizes <- grid::unit.c(tick_size, spacer, labels, title)
    if (isTRUE(params$lab_first)) {
      sizes <- rev(sizes)
    }
    sizes
  }
)

## Total size (perpendicular to the axis) of the label stack, in cm.
.ggse_labels_size_cm <- function(grobs) {
  if (length(grobs) == 0L) {
    return(0)
  }
  sizes <- vapply(grobs, function(g) {
    s <- attr(g, "ggse_size_cm")
    if (is.null(s) || !is.finite(s)) .ggse_grob_size_cm(g, FALSE) else s
  }, numeric(1))
  sum(sizes)
}

## ---------------------------------------------------------------------------
## Label selection
## ---------------------------------------------------------------------------

## Turn the `which` specification into a logical vector over the breaks.
.ggse_selection <- function(which, labels, pos = NULL, n = NULL) {
  m <- length(labels)
  if (m == 0L) {
    return(logical())
  }
  if (is.null(which)) {
    sel <- rep(TRUE, m)
  } else if (is.function(which)) {
    res <- which(labels, pos)
    if (is.logical(res)) {
      if (length(res) != m) {
        cli::cli_abort(c(
          "The function supplied to {.arg which} must return a logical vector of length {m}, or a numeric vector of indices.",
          "x" = "It returned a logical vector of length {length(res)}."
        ))
      }
      sel <- res
    } else if (is.numeric(res)) {
      sel <- rep(FALSE, m)
      sel[as.integer(res)] <- TRUE
    } else {
      cli::cli_abort(
        "The function supplied to {.arg which} must return a logical or numeric vector."
      )
    }
  } else if (length(which) == 0L && is.atomic(which)) {
    ## `logical(0)`, `integer(0)` and `character(0)` all mean "show nothing".
    sel <- rep(FALSE, m)
  } else if (is.logical(which)) {
    if (length(which) != m) {
      cli::cli_abort(c(
        "A logical {.arg which} must have the same length as the number of breaks ({m}).",
        "x" = "It has length {length(which)}."
      ))
    }
    sel <- which
  } else if (is.numeric(which)) {
    ii <- as.integer(which)
    bad <- is.na(ii) | ii < 1L | ii > m
    if (any(bad)) {
      cli::cli_abort(c(
        "All indices in {.arg which} must be between 1 and {m}.",
        "x" = "Invalid indices: {.val {which[bad]}}."
      ))
    }
    sel <- rep(FALSE, m)
    sel[unique(ii)] <- TRUE
  } else if (is.character(which)) {
    sel <- labels %in% which
    ## A faceted plot builds one guide per panel and every panel only holds a
    ## subset of the labels, so unmatched requests (including typos) are simply
    ## skipped instead of warning once per panel.
  } else {
    cli::cli_abort("Unsupported {.arg which} specification.")
  }

  if (!is.null(n) && sum(sel) > n) {
    cand <- which(sel)
    keep <- cand[unique(round(seq(1, length(cand), length.out = n)))]
    sel <- rep(FALSE, m)
    sel[keep] <- TRUE
  }
  sel
}

## Pick (at most) the labels that fit into `capacity` centimetres of track.
## Labels are kept in an evenly spaced fashion so that the first and the last
## label survive.
.ggse_even_subset <- function(cand, width, pad, capacity) {
  k <- length(cand)
  if (k == 0L) {
    return(list(keep = integer(), n = 0L))
  }
  widths <- width[cand]
  total <- function(ii) sum(widths[ii] + pad)
  if (total(seq_len(k)) <= capacity) {
    return(list(keep = cand, n = k))
  }
  fits <- function(m) {
    if (m <= 0L) {
      return(TRUE)
    }
    ii <- unique(round(seq(1, k, length.out = m)))
    total(ii) <= capacity
  }
  lo <- 1L
  hi <- k - 1L
  while (lo < hi) {
    mid <- (lo + hi + 1L) %/% 2L
    if (fits(mid)) lo <- mid else hi <- mid - 1L
  }
  ii <- unique(round(seq(1, k, length.out = lo)))
  list(keep = cand[ii], n = lo)
}

## Choose the number of parallel tracks.
##
## Two requirements are combined:
##
## * capacity -- the labels of one track must fit along the axis at all, so
##   `ink / avail` tracks are needed as a lower bound;
## * compactness -- if all labels of a track are squeezed into one cluster, the
##   track has to fan them out, which produces long leader lines.  Allowing one
##   track to fan out over the labels' own span plus ten percent of the panel is
##   considered acceptable; beyond that another track is added.
##
## This gives a single track (the `ComplexHeatmap::anno_mark()` look: labels are
## spread along the axis only) whenever that is possible, and several tracks
## only for labels that really crowd into one small region.
.ggse_choose_tracks <- function(pos, widths, pad, avail_npc, max_nrow,
                                fan_allowance = 0.25) {
  n_labels <- length(pos)
  if (n_labels == 0L) {
    return(1L)
  }
  max_nrow <- max(1L, min(as.integer(max_nrow), n_labels))
  ink <- sum(widths) + n_labels * pad
  capacity_k <- ceiling(ink / max(avail_npc, 1e-9))
  span <- if (n_labels > 1L) diff(range(pos)) else 0
  compact_k <- ceiling(ink / max(span + fan_allowance, 1e-9))
  k <- max(1L, capacity_k, compact_k)
  as.integer(min(k, max_nrow))
}

## ---------------------------------------------------------------------------
## Preparation: selection, track count, box sizes
## ---------------------------------------------------------------------------

## Everything the label grob and the tick filter need, computed once per guide
## draw.  Returns `NULL` when there is nothing to draw.
.ggse_prepare <- function(key, params, elements) {
  major <- key
  if (".type" %in% names(key)) {
    major <- key[key$.type == "major", , drop = FALSE]
  }
  if (nrow(major) < 1L) {
    return(NULL)
  }
  labels <- as.character(major$.label)
  pos <- as.numeric(major[[params$aes]])
  if (length(pos) != length(labels)) {
    return(NULL)
  }
  vertical <- isTRUE(params$vertical)

  style <- .ggse_text_style(elements$text)
  if (is.null(style)) {
    return(NULL)
  }

  boxes <- .ggse_text_box_cm(
    labels,
    size       = style$size,
    colour     = style$colour,
    family     = style$family,
    face       = style$face,
    lineheight = style$lineheight,
    angle      = style$angle
  )
  box_along <- if (vertical) boxes$height else boxes$width
  box_across <- if (vertical) boxes$width else boxes$height
  box_along[box_along <= 0] <- 0.1
  box_across[box_across <= 0] <- 0.1

  label_h <- max(box_across)
  line_cm <- .ggse_line_height_cm(style$size, style$lineheight)
  pad_cm <- params$padding * line_cm
  gap_cm <- params$gap * line_cm

  ## -- which labels to display -------------------------------------------
  shown <- .ggse_selection(params$which, labels, pos, n = params$n)
  if (!any(shown)) {
    return(NULL)
  }

  ## -- how many tracks ----------------------------------------------------
  dev <- .ggse_device_size_cm()
  if (!is.null(params$panel_width)) {
    panel_est <- params$panel_width
  } else {
    panel_est <- 0.70 * (if (vertical) dev[2] else dev[1])
  }
  if (!is.finite(panel_est) || panel_est <= 0) {
    panel_est <- 14
  }
  avail_cm <- panel_est * (1 + 2 * params$extend)
  max_nrow <- max(1L, min(as.integer(params$max_nrow), sum(shown)))

  ## Width of one track, across the axis: the tallest label box for a
  ## horizontal axis (rotation included) and the widest label for a vertical
  ## axis, where the tracks are columns.
  track_cm <- max(box_across[shown])
  if (!is.finite(track_cm) || track_cm <= 0) {
    track_cm <- line_cm
  }
  row_h_cm <- track_cm + params$gap * line_cm
  if (!is.finite(row_h_cm) || row_h_cm <= 0) {
    row_h_cm <- line_cm
  }

  dropped <- integer()
  if (is.null(params$nrow)) {
    capacity <- max_nrow * avail_cm
    need <- sum(box_along[shown] + pad_cm)
    if (need > capacity) {
      subset <- .ggse_even_subset(which(shown), box_along, pad_cm, capacity)
      keep <- subset$keep
      dropped <- setdiff(which(shown), keep)
      shown[] <- FALSE
      shown[keep] <- TRUE
      if (length(dropped) > 0L) {
        .ggse_inform(c(
          "i" = "Only {sum(shown)} of {sum(shown) + length(dropped)} labels fit into {max_nrow} track{?s}; {length(dropped)} label{?s} {?is/are} not displayed.",
          "i" = "Set {.arg nrow} or {.arg which} in {.fn guide_axis_smart} to control this."
        ))
      }
    }
    ## Pick the number of tracks that keeps the leader lines shortest without
    ## wasting space: labels that sit in a dense cluster are spread over
    ## several tracks instead of being pushed far along the axis.
    nrow_final <- .ggse_choose_tracks(
      pos       = pos[shown],
      widths    = box_along[shown] / panel_est,
      pad       = pad_cm / panel_est,
      avail_npc = avail_cm / panel_est,
      max_nrow  = max_nrow
    )
  } else {
    nrow_final <- max(1L, min(as.integer(params$nrow), sum(shown)))
    need <- sum(box_along[shown] + pad_cm)
    if (need > nrow_final * avail_cm + 1e-9) {
      .ggse_inform(c(
        "!" = "The requested nrow = {nrow_final} is probably too small for {sum(shown)} labels: some of them will not be drawn.",
        "i" = "Increase {.arg nrow}, or select fewer labels with {.arg which} / {.arg n}."
      ))
    }
  }
  nrow_final <- as.integer(max(1L, nrow_final))

  if (isTRUE(getOption("ggutils.debug"))) {
    message(
      "ggutils: labels=", length(labels), " shown=", sum(shown),
      " panel_est=", .ggse_fmt(panel_est), "cm",
      " avail=", .ggse_fmt(avail_cm), "cm",
      " total_width=", .ggse_fmt(sum(box_along[shown] + pad_cm)), "cm",
      " nrow=", nrow_final
    )
  }

  ## Distance between the axis and the anchor of the first label track, in cm.
  ## The leader line occupies this band; the label text starts at its outer end
  ## (same geometry as `ComplexHeatmap::anno_mark()`).
  link_cm <- (params$link_length %||% 3) * 0.1
  if (!is.finite(link_cm) || link_cm < 0) link_cm <- 0
  link_cm <- link_cm + .ggse_mm(1) # room for the small gap before the text
  total_across_cm <- link_cm + nrow_final * row_h_cm

  ## The colour of the leader lines defaults to the colour of the ticks.
  connection_col <- params$connection_col
  if (is.null(connection_col)) {
    tick_el <- elements$ticks
    connection_col <- if (!is.null(tick_el) && !is.null(tick_el$colour)) {
      scales::alpha(tick_el$colour, 0.8)
    } else {
      scales::alpha(style$colour, 0.75)
    }
  }

  list(
    labels            = labels,
    pos               = pos,
    shown             = shown,
    vertical          = vertical,
    position          = params$position,
    nrow              = nrow_final,
    row_h_cm          = row_h_cm,
    link_cm           = link_cm,
    total_across_cm   = total_across_cm,
    box_along_cm      = box_along,
    box_across_cm     = box_across,
    pad_cm            = pad_cm,
    gap_cm            = gap_cm,
    line_cm           = line_cm,
    style             = style,
    extend            = params$extend,
    connection        = isTRUE(params$connection),
    connection_style  = params$connection_style %||% "diagonal",
    connection_col    = connection_col,
    connection_lwd    = params$connection_lwd %||% 0.5,
    connection_lty    = params$connection_lty %||% 1,
    hjust             = params$hjust,
    vjust             = params$vjust,
    dropped           = dropped
  )
}
