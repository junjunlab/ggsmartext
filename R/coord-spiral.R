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

## user-facing angle (degrees, counter-clockwise from east) -> theta.  This is
## the inverse of the `sx`/`sy` used when drawing, so 0 really is to the right
## of the panel whatever `clockwise` / `flip` do.
.ggspiral_angle_to_theta <- function(deg, fr) {
  a <- .ggspiral_deg(deg)
  sx <- cos(a) * fr$dir * (if (isTRUE(fr$flip_h)) -1 else 1)
  sy <- sin(a) * (if (isTRUE(fr$flip_v)) -1 else 1)
  th <- atan2(sx, sy)
  ## take the last crossing of that direction, so `axis_at` lands on the
  ## outermost turn instead of the first one
  th <- fr$theta0 + (th - fr$theta0) %% (2 * pi)
  if (th <= fr$theta1) th <- fr$theta1 - ((fr$theta1 - th) %% (2 * pi))
  th
}

## Warp every position aesthetic, not just `x` and `y`.  ggplot2 hands the
## layer data over in data scale, and layers such as `geom_segment()` and
## `geom_rect()` carry extra columns (`xend`, `xmin`, `ymin`, ...).  Each of
## them is paired with the row's `x` or `y` so that the point lands on the same
## spiral as its neighbours; the edge between two warped points is still a
## straight line, which is invisible as long as the segment is short.
.ggspiral_transform_position <- function(data, fr) {
  if (is.null(data) || !nrow(data)) return(data)
  x <- data$x
  y <- data$y
  if (is.null(x) && is.null(y)) return(data)

  if (!is.null(x) && !is.null(y)) {
    data$x <- .ggspiral_xy(x, y, fr)$x
    data$y <- .ggspiral_xy(x, y, fr)$y
  } else if (!is.null(x)) {
    data$x <- .ggspiral_xy(x, rep(fr$yr[1], length(x)), fr)$x
  } else {
    data$y <- .ggspiral_xy(rep(fr$xr[1], length(y)), y, fr)$y
  }

  ## end points of a segment / corners of a rect
  for (col in c("xend", "xmin", "xmax")) {
    if (!is.null(data[[col]]) && !is.null(y)) {
      data[[col]] <- .ggspiral_xy(data[[col]], y, fr)$x
    }
  }
  for (col in c("yend", "ymin", "ymax")) {
    if (!is.null(data[[col]]) && !is.null(x)) {
      data[[col]] <- .ggspiral_xy(x, data[[col]], fr)$y
    }
  }
  data
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
#' @param turns Number of turns, as a shortcut for `end = start + 360 * turns`.
#'   When supplied it wins over `end`, so `coord_spiral(turns = 5)` always
#'   draws five turns whatever the default `end` is.
#' @param period Number of x units per turn. When supplied, `end` is
#'   recalculated from the x range and `period`.
#' @param clockwise Draw the spiral clockwise? `FALSE` (the default) draws it
#'   counter-clockwise, like `spiralize()`.
#' @param inner,outer Inner and outer radius of the whole spiral, in panel npc
#'   units (0.5 is the panel edge). Together they set how much of the panel the
#'   spiral covers; the distance between two turns follows from the number of
#'   turns.
#' @param gap White gap between two neighbouring turns, as a fraction of their
#'   distance (0 to 1). `gap = 0.3` is the same as `height = 0.7`.
#' @param height Thickness of the data track, as a fraction of the distance
#'   between two neighbouring turns (0 to 1): `0.7` leaves a 30% white gap.
#'   `gap` is the same knob the other way round, so pick whichever reads
#'   better; `gap` wins when both are given.
#' @param scale_by `"angle"` maps equal differences in x to equal differences
#'   in angle; `"curve"` maps them to equal distances along the spiral curve.
#' @param reverse Reverse the y direction inside the track?
#' @param flip One or more of `"none"`, `"horizontal"`, `"vertical"`,
#'   `"both"`; mirrors the whole spiral.
#' @param labels Draw the x (angle) and y (radial) axis labels along the
#'   spiral?
#' @param y_axis Draw the radial (y) axis ruler as well as the angle axis?
#' @param y_axis_theta Angle in degrees at which the radial ruler is placed;
#'   `NULL` follows `side` of [scale_y_spiral_axis()] (the end of the spiral by
#'   default).  Ignored when that scale sets `axis_at`.
#' @param y_axis_n Number of ticks on the radial ruler when the y scale has
#'   no breaks of its own.
#' @param clip Clipping of the panel, passed on to ggplot2.
#' @param xlim,ylim Limits passed on to the coord (usually set on the scales).
#'
#' @details
#' The coordinate draws both axes itself.  Their ticks are kept short enough to
#' sit inside the white gap between two neighbouring turns, the labels stay just
#' outside the ticks, and the number of radial labels follows the thickness of
#' the track, so a thin track gets fewer of them instead of a pile of
#' overlapping text.
#'
#' Every position aesthetic is warped, not only `x` and `y`: `xend` / `yend`,
#' `xmin` / `xmax` and `ymin` / `ymax` are mapped through the same formula.  So
#' `geom_segment()` (arrows included), `geom_rect()`, `geom_ribbon()` and
#' `geom_boxplot()` land on the track, although the edge between two warped
#' points is still a straight line.  Layers without end points
#' (`geom_hline()`, `geom_vline()`, `geom_abline()`) are not supported.
#'
#' Use [scale_x_spiral_axis()] and [scale_y_spiral_axis()] to change where the
#' axes sit, how long their ticks are and how the labels are turned; they
#' otherwise behave like ordinary continuous scales.
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
coord_spiral <- function(start = 90, end = 360 * 3 + 90, turns = NULL,
                         period = NULL, clockwise = FALSE,
                         inner = 0.04, outer = 0.46, height = 0.7,
                         gap = NULL,
                         scale_by = c("angle", "curve"), reverse = FALSE,
                         flip = c("none", "horizontal", "vertical", "both"),
                         labels = TRUE, y_axis = TRUE, y_axis_theta = NULL,
                         y_axis_n = 5L, clip = "off",
                         xlim = NULL, ylim = NULL) {
  scale_by <- match.arg(scale_by)
  flip <- match.arg(flip, several.ok = TRUE)
  if ("none" %in% flip && length(flip) > 1L) flip <- "none"
  if (!is.null(turns)) {
    if (!is.numeric(turns) || length(turns) != 1L || !is.finite(turns) || turns <= 0) {
      cli::cli_abort("{.arg turns} must be a single positive number.")
    }
    end <- start + 360 * turns
  }
  if (!is.numeric(start) || !is.numeric(end) || end <= start) {
    cli::cli_abort("{.arg start} and {.arg end} must be degrees with `end > start`.")
  }
  if (!is.null(gap)) {
    if (!is.numeric(gap) || length(gap) != 1L || gap < 0 || gap >= 1) {
      cli::cli_abort("{.arg gap} must be a number in [0, 1).")
    }
    height <- 1 - gap
  }
  if (!is.numeric(inner) || !is.numeric(outer) || outer <= inner ||
      outer > 0.49 || inner < 0) {
    cli::cli_abort("{.arg inner}/{.arg outer} must satisfy `0 <= inner < outer <= 0.49`.")
  }
  if (!is.numeric(height) || height <= 0 || height > 1) {
    cli::cli_abort("{.arg height} must be a number between 0 and 1.")
  }

  ggproto(NULL, CoordSpiral,
    start = start, end = end, turns = turns, period = period, gap = gap,
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
      y.minor = yi$minor_source, y.labels = yi$labels,
      ## settings handed over by scale_x_spiral_axis() / scale_y_spiral_axis()
      spiral_axis = list(x = scale_x$spiral_axis, y = scale_y$spiral_axis)
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
    .ggspiral_transform_position(data, .ggspiral_frame(panel_params, self))
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
    line_el <- function(name) {
      el <- ggplot2::calc_element(name, theme)
      if (inherits(el, "element_blank") || is.null(el)) return(NULL)
      grid::gpar(col = el$colour %||% "grey80",
                 lwd = el$linewidth %||% el$size %||% 0.5,
                 lty = el$linetype %||% 1)
    }
    spiral_path <- function(theta, r) {
      sx <- sin(theta) * fr$dir
      sy <- cos(theta)
      if (isTRUE(fr$flip_h)) sx <- -sx
      if (isTRUE(fr$flip_v)) sy <- -sy
      list(x = 0.5 + r * sx, y = 0.5 + r * sy)
    }

    kids <- list()
    ## the canvas is white, the data track is painted with the theme's panel
    ## background and the grid inside it with the theme's grid colour, so the
    ## default theme_grey() look survives the bend into a spiral
    plot_bg <- ggplot2::calc_element("plot.background", theme)
    kids[["background"]] <- grid::rectGrob(
      gp = grid::gpar(fill = plot_bg$fill %||% "white", col = NA)
    )
    bg_el <- ggplot2::calc_element("panel.background", theme)
    band_fill <- if (inherits(bg_el, "element_blank")) "grey92" else bg_el$fill %||% "grey92"

    theta <- seq(fr$theta0, fr$theta1, length.out = 400L)
    xr <- panel_params$x.major
    xr <- xr[!is.na(xr)]

    ## the track is a band between two spirals: fill it first, then draw the
    ## grid and the borders on top
    r_in <- fr$inner + fr$gap * (theta - fr$theta0)
    r_out <- r_in + fr$height
    p_in <- spiral_path(theta, r_in)
    p_out <- spiral_path(theta, r_out)
    kids[["track"]] <- grid::pathGrob(
      x = c(p_in$x, rev(p_out$x)), y = c(p_in$y, rev(p_out$y)),
      gp = grid::gpar(fill = band_fill, col = NA)
    )

    ## vertical grid lines become radial spokes at the x breaks
    if (length(xr)) {
      gx <- line_el("panel.grid.major.x")
      if (!is.null(gx)) {
        tx <- .ggspiral_theta(xr, fr)
        r0 <- fr$inner + fr$gap * (tx - fr$theta0)
        p0 <- spiral_path(tx, r0)
        p1 <- spiral_path(tx, r0 + fr$height)
        kids[["grid_x"]] <- grid::segmentsGrob(
          x0 = p0$x, y0 = p0$y, x1 = p1$x, y1 = p1$y, gp = gx
        )
      }
    }

    ## horizontal grid lines become spirals offset by the y position
    yr <- panel_params$y.major
    yr <- yr[!is.na(yr)]
    if (length(yr)) {
      gy <- line_el("panel.grid.major.y")
      if (!is.null(gy)) {
        yf <- (yr - fr$yr[1]) / if (diff(fr$yr) == 0) 1 else diff(fr$yr)
        yf <- pmin(pmax(yf, 0), 1)
        g <- grid::gList()
        for (i in seq_along(yf)) {
          p <- spiral_path(theta, r_in + yf[i] * fr$height)
          g[[i]] <- grid::polylineGrob(x = p$x, y = p$y, gp = gy,
                                       id = rep(i, length(p$x)))
        }
        kids[["grid_y"]] <- do.call(grid::grobTree, g)
      }
    }

    ## the two borders of the track
    border_gp <- line_el("panel.border") %||% grid::gpar(col = "grey80", lwd = 0.5)
    if (inherits(ggplot2::calc_element("panel.border", theme), "element_blank")) {
      border_gp <- grid::gpar(col = "grey80", lwd = 0.5)
    }
    kids[["border_in"]] <- grid::polylineGrob(x = p_in$x, y = p_in$y, gp = border_gp)
    kids[["border_out"]] <- grid::polylineGrob(x = p_out$x, y = p_out$y, gp = border_gp)

    ## settings coming from scale_x_spiral_axis() / scale_y_spiral_axis()
    ax_x <- panel_params$spiral_axis$x %||% list()
    ax_y <- panel_params$spiral_axis$y %||% list()
    y_axis <- isTRUE(self$y_axis) && isTRUE(self$show_labels) &&
      !identical(ax_y$show, FALSE)

    ## where the radial ruler goes, `spiralize::spiral_yaxis()` style: at the
    ## start of the spiral, at its end, or on both sides.  `outward` is the
    ## direction in which the ticks grow, away from the track.
    ypos <- list()
    if (y_axis) {
      if (!is.null(ax_y$axis_at)) {
        ypos <- list(list(theta = .ggspiral_angle_to_theta(ax_y$axis_at, fr),
                          outward = 1))
      } else if (!is.null(self$y_axis_theta)) {
        ypos <- list(list(theta = .ggspiral_deg(self$y_axis_theta), outward = 1))
      } else {
        side <- ax_y$side %||% "end"
        if (side %in% c("start", "both")) {
          ypos <- c(ypos, list(list(theta = fr$theta0, outward = -1)))
        }
        if (side %in% c("end", "both")) {
          ypos <- c(ypos, list(list(theta = fr$theta1, outward = 1)))
        }
      }
    }

    if (isTRUE(self$show_labels) && !identical(ax_x$show, FALSE) &&
        length(panel_params$x.major) && length(panel_params$x.labels)) {
      ## x (angle) ticks and labels: radial stubs on the outer edge of the track
      tx <- .ggspiral_theta(panel_params$x.major, fr)
      ## labels sitting under a radial ruler would collide with it.  Compare
      ## absolute angles along the spiral: a label one whole turn away is at the
      ## same direction but a different radius, so it does not collide.
      keep <- rep(TRUE, length(tx))
      for (p in ypos) keep <- keep & abs(tx - p$theta) > 0.14
      if (!any(keep)) keep <- rep(TRUE, length(tx))
      xl <- as.character(panel_params$x.labels)[keep]
      tx <- tx[keep]
      tr <- fr$inner + fr$gap * (tx - fr$theta0) + fr$height
      stx <- sin(tx) * fr$dir
      sty <- cos(tx)
      if (isTRUE(fr$flip_h)) stx <- -stx
      if (isTRUE(fr$flip_v)) sty <- -sty
      ## the white gap between two turns is all the room there is: keep the
      ## tick and the label inside it so neither runs into the next band
      gap_npc <- max(fr$loop - fr$height, 0.004)
      stub <- min(ax_x$tick_length %||% 0.007, gap_npc * 0.3)
      ## the label is centred on its anchor, so nudge it out by a bit less
      ## than half a line: just enough air between the glyphs and the tick,
      ## like the spiralize axis, not a gaping hole
      size_x <- ax_x$label_size %||%
        ggplot2::calc_element("axis.text.x", theme)$size %||% 8.8
      half_line <- size_x / 72 / 6 * 0.62      # npc, assuming a 6 inch panel
      lab_off <- min(stub + half_line * 0.55 + 0.0015, gap_npc * 0.72)
      tick_el <- ggplot2::calc_element("axis.ticks.x", theme)
      if (stub > 0 && length(tx)) {
        kids[["x_ticks"]] <- grid::segmentsGrob(
          x0 = 0.5 + tr * stx, y0 = 0.5 + tr * sty,
          x1 = 0.5 + (tr + stub) * stx, y1 = 0.5 + (tr + stub) * sty,
          gp = grid::gpar(col = tick_el$colour %||% "grey20",
                          lwd = tick_el$linewidth %||% 0.5)
        )
      }
      ## labels stand perpendicular to their tick, i.e. along the tangent.
      ## The tangent is measured on the drawn path, so `clockwise` and `flip`
      ## are accounted for (a fixed `-theta` formula is only right for one
      ## direction).  Text is flipped by 180 when that keeps it upright.
      tan_pt <- spiral_path(tx + 1e-4, tr)
      tan_ang <- atan2(tan_pt$y - (0.5 + tr * sty), tan_pt$x - (0.5 + tr * stx)) * 180 / pi
      rot <- tan_ang
      rot <- ifelse(abs(rot) > 90, rot - 180 * sign(rot), rot)
      if (!is.null(ax_x$label_rot)) rot <- ax_x$label_rot
      ## minor ticks: the same radial stub, shorter and thinner, no label
      xmi <- panel_params$x.minor
      if (length(xmi) && stub > 0) {
        tmi <- .ggspiral_theta(xmi, fr)
        rmi <- fr$inner + fr$gap * (tmi - fr$theta0) + fr$height
        smx <- sin(tmi) * fr$dir
        smy <- cos(tmi)
        if (isTRUE(fr$flip_h)) smx <- -smx
        if (isTRUE(fr$flip_v)) smy <- -smy
        keepm <- rep(TRUE, length(tmi))
        for (p in ypos) keepm <- keepm & abs(tmi - p$theta) > 0.14
        if (any(keepm)) {
          kids[["x_ticks_minor"]] <- grid::segmentsGrob(
            x0 = 0.5 + rmi[keepm] * smx[keepm],
            y0 = 0.5 + rmi[keepm] * smy[keepm],
            x1 = 0.5 + (rmi[keepm] + stub * 0.55) * smx[keepm],
            y1 = 0.5 + (rmi[keepm] + stub * 0.55) * smy[keepm],
            gp = grid::gpar(col = tick_el$colour %||% "grey20",
                            lwd = (tick_el$linewidth %||% 0.5) * 0.7)
          )
        }
      }
      if (length(xl)) kids[["x_labels"]] <- ggplot2::element_grob(
        ggplot2::calc_element("axis.text.x", theme),
        label = xl,
        x = 0.5 + (tr + lab_off) * stx,
        y = 0.5 + (tr + lab_off) * sty,
        hjust = 0.5, vjust = 0.5,
        angle = rot, size = ax_x$label_size
      )
    }

    ## the radial ruler, rewritten from `spiralize::spiral_yaxis()`:
    ##  * one tick per break, a short segment running *along* the spiral
    ##    (circumferential), growing outwards from the track;
    ##  * the label sits just past its tick, turned to the tangent at that
    ##    point, and flipped by 180 degrees (with the matching justification)
    ##    when the tangent would leave it upside down or growing inwards.
    if (length(ypos) && isTRUE(self$show_labels)) {
      yv <- yr
      if (is.null(yv) || !length(yv)) {
        yv <- seq(fr$yr[1], fr$yr[2], length.out = self$y_axis_n)
      }
      ylab <- panel_params$y.labels
      if (length(ylab) != length(yv)) ylab <- format(yv, trim = TRUE, digits = 3)
      ok <- yv >= fr$yr[1] & yv <= fr$yr[2]
      yv <- yv[ok]
      ylab <- ylab[ok]
      ## how many labels fit is pure geometry: each one needs about a line of
      ## text across the track, so a thin track holds one or two, not three.
      ## `max_labels` overrides the guess.
      txt_el0 <- ggplot2::calc_element("axis.text.y", theme)
      size0 <- ax_y$label_size %||% txt_el0$size %||% 8
      ## how tall the panel really is, in inches: `npc` alone carries no size,
      ## so ask the device.  The panel is roughly 60% of the device, and being
      ## a little pessimistic is what keeps small panels from over-labelling.
      dev_h <- tryCatch(
        grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE),
        error = function(e) 4.5 / 0.6)
      panel_h <- max(dev_h * 0.6, 1)
      need <- size0 / 72 / panel_h * 1.15      # npc of track a label needs
      fits <- max(1L, floor(fr$height / need))
      mx <- ax_y$max_labels
      if (is.null(mx)) mx <- min(3L, fits)
      if (!is.null(mx) && length(yv) > mx) {
        idx <- if (mx == 1L) round((length(yv) + 1) / 2) else
          round(seq(1, length(yv), length.out = mx))
        yv <- yv[idx]
        ylab <- ylab[idx]
      }

      gap_npc <- max(fr$loop - fr$height, 0.004)
      tlen <- if (is.null(ax_y$tick_length)) 0.008 else ax_y$tick_length
      tlen <- min(tlen, gap_npc * 0.35)
      tick_gp <- grid::gpar(
        col = (ggplot2::calc_element("axis.ticks.x", theme)$colour %||% "grey30"),
        lwd = (ggplot2::calc_element("axis.ticks.x", theme)$linewidth %||% 0.5))
      txt_el <- ggplot2::calc_element("axis.text.y", theme)
      size <- ax_y$label_size %||% txt_el$size %||% 8
      ## the text box already carries some leading space, so a tiny offset is
      ## enough to match the hairline the angle labels get
      gap <- min(0.0015, gap_npc * 0.5)

      if (length(yv)) {
        yf <- pmin(pmax((yv - fr$yr[1]) / if (diff(fr$yr) == 0) 1 else diff(fr$yr), 0), 1)
        for (i in seq_along(ypos)) {
          ta <- ypos[[i]]$theta
          sgn <- ypos[[i]]$outward
          rr <- fr$inner + fr$gap * (ta - fr$theta0) + yf * fr$height
          dt <- tlen / pmax(rr, 0.06)
          p0 <- spiral_path(ta, rr)
          p1 <- spiral_path(ta + sgn * dt, rr)
          p2 <- spiral_path(ta + sgn * (tlen + gap) / pmax(rr, 0.06), rr)
          ## tangent at the label, taken from the drawing itself so that
          ## `clockwise` / `flip` are already accounted for
          p3 <- spiral_path(ta + sgn * (tlen + gap + 0.01) / pmax(rr, 0.06), rr)
          ang <- atan2(p3$y - p2$y, p3$x - p2$x) * 180 / pi
          flip <- abs(ang) > 90
          rot <- ifelse(flip, ang - 180 * sign(ang), ang)
          hjust <- ifelse(flip, 1, 0)
          if (!is.null(ax_y$label_rot)) {
            rot <- ax_y$label_rot
            hjust <- 0
          }

          one <- list()
          if (tlen > 0) {
            one[["y_ticks"]] <- grid::segmentsGrob(
              x0 = p0$x, y0 = p0$y, x1 = p1$x, y1 = p1$y, gp = tick_gp)
          }
          ## a white halo keeps the labels readable where they cross the track
          one[["y_halo"]] <- grid::textGrob(
            label = ylab, x = p2$x, y = p2$y, hjust = hjust, vjust = 0.5,
            rot = rot, gp = grid::gpar(col = "white", fontsize = size + 1.6)
          )
          one[["y_labels"]] <- ggplot2::element_grob(
            txt_el, label = ylab, x = p2$x, y = p2$y,
            hjust = hjust, vjust = 0.5, angle = rot,
            size = ax_y$label_size
          )
          one <- lapply(names(one), function(nm) {
            one[[nm]]$name <- nm
            one[[nm]]
          })
          kids[[paste0("y_axis", i)]] <- grid::gTree(
            children = do.call(grid::gList, one))
        }
      }
    }

    ## name the grobs themselves: `grobTree()` would rename them, and renaming
    ## the children of an existing gTree afterwards stops it from drawing
    kids <- lapply(seq_along(kids), function(i) {
      kids[[i]]$name <- names(kids)[i]
      kids[[i]]
    })
    grid::gTree(children = do.call(grid::gList, kids))
  },

  labels = function(self, labels, panel_params) labels
)
