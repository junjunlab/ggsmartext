## ---------------------------------------------------------------------------
## coord_spiral(): map a Cartesian ggplot onto an Archimedean spiral
## ---------------------------------------------------------------------------
## The geometry follows spiralize (jokergoo/spiralize): r = b * theta with
## b chosen so that one loop corresponds to one "unit" of radius.  Data on the
## x axis are mapped to the angle, data on the y axis to the radius inside a
## track that follows the spiral.  Facets work because every panel gets its
## own `panel_params` from ggplot2.

.ggspiral_deg <- function(x) x * pi / 180

## Arc length of r = u + b, integrated from 0 to `u`: the antiderivative of
## sqrt(u^2 + b^2) is (u * sqrt(u^2 + b^2) + b^2 * log(u + sqrt(u^2 + b^2))) / 2.
.ggspiral_arclen <- function(u, b) {
  (u * sqrt(u^2 + b^2) + b^2 * log(u + sqrt(u^2 + b^2))) / 2
}

## Panel parameters -> complete spiral description.  Computed once per panel,
## which is what makes facet_wrap()/facet_grid() (also with free scales) work.
.ggspiral_frame <- function(panel_params, self) {
  xr <- panel_params$x.range
  yr <- panel_params$y.range
  if (!is.null(self$limits$x)) xr <- self$limits$x
  if (!is.null(self$limits$y)) yr <- self$limits$y

  theta0 <- .ggspiral_deg(self$start)
  theta1 <- .ggspiral_deg(self$end)
  if (!is.null(self$period) && is.finite(self$period) && self$period > 0) {
    theta1 <- theta0 + 2 * pi * diff(xr) / self$period
  }
  if (theta1 <= theta0) theta1 <- theta0 + 2 * pi

  inner <- self$inner
  outer <- self$outer
  ## leave room for both the turns and the track height, otherwise the last
  ## turn (plus the data track) would overshoot the panel edge
  h <- min(max(self$height, 0.01), 1)
  gap <- (outer - inner) / ((theta1 - theta0) + 2 * pi * h)
  loop <- 2 * pi * gap                          # radial distance between loops
  height <- h * loop

  list(xr = xr, yr = yr, theta0 = theta0, theta1 = theta1,
       inner = inner, outer = outer, gap = gap, loop = loop, height = height,
       dir = if (isTRUE(self$clockwise)) 1 else -1,
       flip_h = "horizontal" %in% self$flip || "both" %in% self$flip,
       flip_v = "vertical" %in% self$flip || "both" %in% self$flip,
       scale_by = self$scale_by, reverse = isTRUE(self$reverse))
}

## x (data scale) -> angle in radians
.ggspiral_theta <- function(x, fr) {
  f <- (x - fr$xr[1]) / (fr$xr[2] - fr$xr[1])
  f <- pmin(pmax(f, 0), 1)
  if (identical(fr$scale_by, "curve")) {
    ## equal steps in arc length rather than equal steps in angle
    grid <- seq(fr$theta0, fr$theta1, length.out = 256L)
    u <- grid - fr$theta0
    s <- .ggspiral_arclen(u, fr$gap)
    return(stats::approx(s, grid, xout = fr$theta0 + f * tail(s, 1),
                         rule = 2)$y)
  }
  fr$theta0 + f * (fr$theta1 - fr$theta0)
}

## (x, y) in data scale -> (x, y) in panel npc
.ggspiral_xy <- function(x, y, fr) {
  theta <- .ggspiral_theta(x, fr)
  yf <- if (diff(fr$yr) == 0) rep(0.5, length(y)) else
    (y - fr$yr[1]) / diff(fr$yr)
  yf <- pmin(pmax(yf, 0), 1)
  if (isTRUE(fr$reverse)) yf <- 1 - yf
  r <- fr$inner + fr$gap * (theta - fr$theta0) + yf * fr$height
  sx <- sin(theta) * fr$dir
  sy <- cos(theta)
  if (isTRUE(fr$flip_h)) sx <- -sx
  if (isTRUE(fr$flip_v)) sy <- -sy
  list(x = 0.5 + r * sx, y = 0.5 + r * sy, theta = theta, r = r)
}

#' Spiral coordinates for ggplot2
#'
#' `coord_spiral()` bends a normal Cartesian ggplot into an Archimedean
#' spiral, following the geometry used by the **spiralize** package: the
#' base spiral is `r = b * theta` with `b` chosen so that one turn is one
#' radius unit.  The x aesthetic runs along the spiral (as the angle) and the
#' y aesthetic runs across the track (as the radius).  Points, lines, bars and
#' areas are all supported, and facets get one spiral per panel.
#'
#' @param start,end Angular range of the spiral, **in degrees** (as in
#'   `spiralize::spiral_initialize()`). `360 * n + a` is a convenient way to
#'   ask for `n` turns ending at angle `a`.
#' @param period Number of x units per turn. When supplied, `end` is
#'   recalculated from the x range and `period`.
#' @param clockwise Draw the spiral clockwise? `FALSE` (the default) draws it
#'   counter-clockwise, like `spiralize()`.
#' @param inner,outer Inner and outer radius of the whole spiral, in panel npc
#'   units (0.5 is the panel edge). The distance between two turns is
#'   `(outer - inner) / (end - start) * 360`.
#' @param height Height of the data track as a fraction of the distance
#'   between two neighbouring turns (0 to 1).
#' @param scale_by `"angle"` maps equal differences in x to equal differences
#'   in angle; `"curve"` maps them to equal distances along the spiral curve.
#' @param reverse Reverse the y direction inside the track?
#' @param flip One or more of `"none"`, `"horizontal"`, `"vertical"`,
#'   `"both"`; mirrors the whole spiral.
#' @param labels Draw the x (angle) and y (radial) axis labels along the
#'   spiral?
#' @param y_axis Draw the radial (y) axis ruler as well as the angle axis?
#' @param y_axis_theta Angle in degrees at which the radial ruler is placed;
#'   `NULL` puts it at the outermost end of the spiral.
#' @param y_axis_n Number of ticks on the radial ruler when the y scale has
#'   no breaks of its own.
#' @param clip Clipping of the panel, passed on to ggplot2.
#' @param xlim,ylim Limits passed on to the coord (usually set on the scales).
#'
#' @return A `CoordSpiral` ggproto object.
#' @export
#' @examples
#' library(ggplot2)
#' d <- data.frame(x = seq(0, 1, length.out = 200),
#'                 y = sin(seq(0, 1, length.out = 200) * 8 * pi))
#' ggplot(d, aes(x, y)) + geom_line() + coord_spiral()
#'
#' ggplot(d[1:60, ], aes(x, y)) +
#'   geom_col() + coord_spiral(end = 360 + 90, height = 0.8)
coord_spiral <- function(start = 90, end = 360 * 3 + 90,
                         period = NULL, clockwise = FALSE,
                         inner = 0.04, outer = 0.46, height = 0.8,
                         scale_by = c("angle", "curve"), reverse = FALSE,
                         flip = c("none", "horizontal", "vertical", "both"),
                         labels = TRUE, y_axis = TRUE, y_axis_theta = NULL,
                         y_axis_n = 5L, clip = "off",
                         xlim = NULL, ylim = NULL) {
  scale_by <- match.arg(scale_by)
  flip <- match.arg(flip, several.ok = TRUE)
  if ("none" %in% flip && length(flip) > 1L) flip <- "none"
  if (!is.numeric(start) || !is.numeric(end) || end <= start) {
    cli::cli_abort("{.arg start} and {.arg end} must be degrees with `end > start`.")
  }
  if (!is.numeric(inner) || !is.numeric(outer) || outer <= inner ||
      outer > 0.49 || inner < 0) {
    cli::cli_abort("{.arg inner}/{.arg outer} must satisfy `0 <= inner < outer <= 0.49`.")
  }
  if (!is.numeric(height) || height <= 0 || height > 1) {
    cli::cli_abort("{.arg height} must be a number between 0 and 1.")
  }

  ggproto(NULL, CoordSpiral,
    start = start, end = end, period = period,
    clockwise = clockwise, inner = inner, outer = outer, height = height,
    scale_by = scale_by, reverse = reverse, flip = flip,
    show_labels = labels, y_axis = y_axis, y_axis_theta = y_axis_theta,
    y_axis_n = y_axis_n, clip = clip,
    limits = list(x = xlim, y = ylim)
  )
}

#' @rdname coord_spiral
#' @format NULL
#' @usage NULL
#' @export
CoordSpiral <- ggplot2::ggproto("CoordSpiral", ggplot2::Coord,

  aspect = function(details) 1,
  is_free = function() TRUE,
  is_linear = function() FALSE,

  range = function(self, panel_params) {
    list(x = panel_params$x.range, y = panel_params$y.range)
  },

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    xr <- ggplot2:::expand_limits_scale(scale_x, ggplot2::expansion(0, 0),
                                        coord_limits = self$limits$x)
    yr <- ggplot2:::expand_limits_scale(scale_y, ggplot2::expansion(0, 0),
                                        coord_limits = self$limits$y)
    xi <- scale_x$break_info(xr)
    yi <- scale_y$break_info(yr)
    list(
      x.range = xi$range, y.range = yi$range,
      x.major = xi$major_source, x.minor = xi$minor_source,
      x.labels = xi$labels, y.major = yi$major_source,
      y.minor = yi$minor_source, y.labels = yi$labels
    )
  },

  ## axis guides are drawn along the spiral in render_bg(), not on the panel
  ## sides, so the stock x/y guides are omitted
  setup_panel_guides = function(self, panel_params, guides, params = list()) {
    panel_params
  },
  train_panel_guides = function(self, panel_params, layers, params = list()) {
    panel_params
  },

  ## the spiral has no conventional axis sides, so drop the x/y axis titles
  ## (users can still add a plot title with labs(title = ))
  modify_scales = function(self, scales_x, scales_y) {
    lapply(scales_x, function(s) s$name <- NULL)
    lapply(scales_y, function(s) s$name <- NULL)
  },

  transform = function(self, data, panel_params) {
    fr <- .ggspiral_frame(panel_params, self)
    xy <- .ggspiral_xy(data$x, data$y, fr)
    data$x <- xy$x
    data$y <- xy$y
    data
  },

  ## distance is used by ggplot2's munching: segments are split until their
  ## length in *spiral* space is small enough, so lines and areas follow the
  ## curve instead of cutting across it.
  distance = function(self, x, y, details, boost = 0.75) {
    pp <- list(x.range = details$x.range, y.range = details$y.range)
    fr <- .ggspiral_frame(pp, self)
    xy <- .ggspiral_xy(x, y, fr)
    n <- length(xy$x)
    if (n < 2L) return(numeric())
    sqrt(diff(xy$x)^2 + diff(xy$y)^2) * boost
  },

  render_axis_h = function(self, panel_params, theme) {
    list(top = grid::nullGrob(), bottom = grid::nullGrob())
  },
  render_axis_v = function(self, panel_params, theme) {
    list(left = grid::nullGrob(), right = grid::nullGrob())
  },

  render_fg = function(self, panel_params, theme) grid::nullGrob(),

  render_bg = function(self, panel_params, theme) {
    fr <- .ggspiral_frame(panel_params, self)
    theta <- seq(fr$theta0, fr$theta1, length.out = 400L)
    r <- fr$inner + fr$gap * (theta - fr$theta0)
    sx <- sin(theta) * fr$dir
    sy <- cos(theta)
    if (isTRUE(fr$flip_h)) sx <- -sx
    if (isTRUE(fr$flip_v)) sy <- -sy

    line_gp <- grid::gpar(col = ggplot2::calc_element("panel.grid.major", theme)$colour %||% "grey80",
                          lwd = 0.6)
    base <- grid::polylineGrob(
      x = 0.5 + r * sx, y = 0.5 + r * sy, gp = line_gp
    )
    y_axis <- isTRUE(self$y_axis) && isTRUE(self$show_labels)
    if (!isTRUE(self$show_labels) && !y_axis) return(grid::grobTree(base))
    kids <- list(base)

    if (isTRUE(self$show_labels) &&
        length(panel_params$x.major) && length(panel_params$x.labels)) {
      ## x (angle) ticks and labels: radial stubs on the outer edge of the track
    tx <- .ggspiral_theta(panel_params$x.major, fr)
    tr <- fr$inner + fr$gap * (tx - fr$theta0) + fr$height
    stx <- sin(tx) * fr$dir
    sty <- cos(tx)
    if (isTRUE(fr$flip_h)) stx <- -stx
    if (isTRUE(fr$flip_v)) sty <- -sty
    stub <- 0.012
    tick <- grid::segmentsGrob(
      x0 = 0.5 + tr * stx, y0 = 0.5 + tr * sty,
      x1 = 0.5 + (tr + stub) * stx, y1 = 0.5 + (tr + stub) * sty,
      gp = grid::gpar(col = "grey50", lwd = 0.5)
    )
    txt <- ggplot2::element_grob(
      ggplot2::calc_element("axis.text.x", theme),
      label = as.character(panel_params$x.labels),
      x = 0.5 + (tr + stub * 2) * stx,
      y = 0.5 + (tr + stub * 2) * sty,
      hjust = ifelse(stx > 0, 0, ifelse(stx < 0, 1, 0.5)),
      vjust = ifelse(sty > 0, 0, ifelse(sty < 0, 1, 0.5))
    )
    kids <- c(kids, list(tick, txt))
    }

    ## y axis: a radial ruler at `y_axis_theta`.  A track is thin when the
    ## spiral has many turns, so the labels are fanned out tangentially and
    ## connected back to the ticks with short leaders.
    if (y_axis) {
      ta <- if (is.null(self$y_axis_theta)) fr$theta1 else .ggspiral_deg(self$y_axis_theta)
      yv <- panel_params$y.major
      if (is.null(yv) || !length(yv)) {
        yv <- seq(fr$yr[1], fr$yr[2], length.out = self$y_axis_n)
      }
      yv <- yv[yv >= fr$yr[1] & yv <= fr$yr[2]]
      if (length(yv)) {
        yf <- pmin(pmax((yv - fr$yr[1]) / diff(fr$yr), 0), 1)
        rr <- fr$inner + fr$gap * (ta - fr$theta0) + yf * fr$height
        sx <- sin(ta) * fr$dir
        sy <- cos(ta)
        if (isTRUE(fr$flip_h)) sx <- -sx
        if (isTRUE(fr$flip_v)) sy <- -sy
        kids[["y_ticks"]] <- grid::segmentsGrob(
          x0 = 0.5 + rr * sx, y0 = 0.5 + rr * sy,
          x1 = 0.5 + (rr + 0.012) * sx, y1 = 0.5 + (rr + 0.012) * sy,
          gp = grid::gpar(col = "grey50", lwd = 0.5)
        )
        ## fan the labels so neighbouring values never sit on top of each other
        m <- length(yv)
        ang <- ta + (seq_len(m) - (m + 1) / 2) * 0.035 * fr$dir
        lx <- sin(ang) * fr$dir
        ly <- cos(ang)
        if (isTRUE(fr$flip_h)) lx <- -lx
        if (isTRUE(fr$flip_v)) ly <- -ly
        lab_r <- pmax(rr + 0.02, fr$outer + 0.02)
        kids[["y_leaders"]] <- grid::segmentsGrob(
          x0 = 0.5 + (rr + 0.012) * sx, y0 = 0.5 + (rr + 0.012) * sy,
          x1 = 0.5 + lab_r * lx, y1 = 0.5 + lab_r * ly,
          gp = grid::gpar(col = "grey70", lwd = 0.4)
        )
        kids[["y_labels"]] <- grid::textGrob(
          label = format(yv, trim = TRUE, digits = 3),
          x = 0.5 + lab_r * lx, y = 0.5 + lab_r * ly,
          hjust = ifelse(lx > 0.1, 0, ifelse(lx < -0.1, 1, 0.5)),
          vjust = ifelse(ly > 0.1, 0, ifelse(ly < -0.1, 1, 0.5)),
          gp = grid::gpar(col = "grey25", fontsize = 8)
        )
      }
    }

    do.call(grid::grobTree, kids)
  },

  labels = function(self, labels, panel_params) labels
)
