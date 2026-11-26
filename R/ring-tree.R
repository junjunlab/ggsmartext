## ---------------------------------------------------------------------------
## Ring dendrogram: cluster trees for a `coord_radial()` theta axis
## ---------------------------------------------------------------------------
## A theta axis runs around a circle, so the Cartesian tree (a strip above /
## beside the panel) has no place to live.  The tree is drawn as a *ring
## dendrogram* instead: leaves sit on a circle just outside the axis labels and
## every merge is an arc at a larger radius, joined to its children by radial
## stubs.  The tree is drawn into the finished gtable, one grob per panel, which
## is also what makes `facet_wrap()` / `facet_grid()` work per panel.

## Angle of every category of a radial theta scale.  `theta.range` is the data
## range of the (discrete) theta scale; the coordinate maps it linearly onto the
## drawn arc, which was verified against `CoordRadial$transform()`.
.ggcl_theta_break_angles <- function(pp) {
  view <- pp$theta
  if (is.null(view)) return(NULL)
  breaks <- as.character(view$get_breaks())
  if (length(breaks) < 2L) return(NULL)
  cr <- view$continuous_range
  if (is.null(cr) || length(cr) != 2L || !all(is.finite(cr)) ||
      diff(cr) == 0) {
    cr <- c(0.5, length(breaks) + 0.5)
  }
  arc <- pp$arc %||% c(0, 2 * pi)
  span <- diff(arc)
  if (!is.finite(span) || span <= 0) {
    arc <- c(0, 2 * pi)
    span <- 2 * pi
  }
  frac <- (seq_along(breaks) - cr[1]) / diff(cr)
  list(
    labels = breaks,
    angles = arc[1] + frac * span,
    arc = arc,
    span = span,
    ## Partial arcs (`start`/`end`) are drawn as a sector: `coord_radial()`
    ## rescales the pre-bbox circle `0.5 + r * sin/cos` through this box, so the
    ## centre is no longer (0.5, 0.5) in panel npc.
    bbox = pp$bbox %||% list(x = c(0, 1), y = c(0, 1))
  )
}

## hclust for one panel.  Mirrors `.ggcl_tree_grob_for_panel()`: free facets use
## the per-panel tree the scale built while re-keying the axis, a supplied
## matrix is subset to the panel's rows, and a fixed scale shares one tree.
.ggcl_ring_hclust_for_panel <- function(pp, spec) {
  view <- pp$theta
  if (is.null(view)) return(NULL)
  breaks <- as.character(view$get_breaks())
  hc <- NULL
  if (isTRUE(spec$free)) {
    parts <- strsplit(breaks[1], .ggcl_sep, fixed = TRUE)[[1]]
    key <- paste(parts[-length(parts)], collapse = .ggcl_sep)
    hc <- spec$panel_hclust[[key]]
  }
  if (is.null(hc) || !inherits(hc, "hclust")) hc <- spec$hclust
  if ((is.null(hc) || !inherits(hc, "hclust")) && !is.null(spec$mat)) {
    idx <- match(.ggcl_strip_key(breaks), rownames(spec$mat))
    if (!anyNA(idx) && length(idx) >= 2L) {
      method <- spec$method %||% "complete"
      hc <- stats::hclust(stats::dist(spec$mat[idx, , drop = FALSE]),
                          method = method)
      hc$labels <- rownames(spec$mat)[idx]
    }
  }
  if (is.null(hc) || !inherits(hc, "hclust")) return(NULL)
  hc
}

## Radius (in cm, beyond the circle) of a height value.  Leaves sit at 0, the
## tallest merge at `height` cm.
.ggcl_ring_radius_cm <- function(h, height_cm) {
  h <- as.numeric(h)
  hmax <- max(c(h, 1e-8))
  (h / hmax) * height_cm
}

## Convert a set of (angle, radius-cm) points into `grid` units around the panel
## centre.  `base_cm` is the gap between the circle and the leaf ring.
## Measure the drawn circle in panel npc from the coordinate transform itself.
##
## `coord_radial()` builds the circle in "pre-bbox" coordinates (outer radius
## 0.4, not 0.5) and then rescales that box to the panel; a partial arc
## (`start`/`end`) has a smaller box, so neither the centre nor the radius is a
## constant in panel npc.  Sampling the outer ring through `coord$transform()`
## and taking the extents of those points gives the centre and the per-axis
## radii exactly, without re-implementing ggplot2's polar mapping.
.ggcl_theta_circle <- function(pp, coord) {
  default <- list(cx = 0.5, cy = 0.5, rx = 0.5, ry = 0.5)
  if (is.null(coord) || is.null(pp$r.range) || is.null(pp$theta)) return(default)
  theta_pos <- pp$theta$continuous_range %||% pp$theta.range
  arc <- pp$arc %||% c(0, 2 * pi)
  if (is.null(theta_pos) || length(theta_pos) != 2L) return(default)
  span <- diff(arc)
  if (!is.finite(span) || span <= 0) return(default)
  r_outer <- pp$r.range[2]
  if (!is.finite(r_outer)) return(default)

  ## Sample enough theta positions to cover a full turn (a partial arc needs a
  ## proportionally wider position range), then transform them to panel npc.
  turns <- min(4, max(1, ceiling(2 * pi / span)))
  frac <- seq(0, turns, length.out = 145)
  xs <- theta_pos[1] + frac * diff(theta_pos)
  df <- data.frame(x = xs, y = rep(r_outer, length(xs)))
  tr <- tryCatch(coord$transform(df, pp), error = function(e) NULL)
  if (is.null(tr) || !all(c("x", "y") %in% names(tr))) return(default)
  pts <- cbind(tr$x, tr$y)
  pts <- pts[is.finite(pts[, 1]) & is.finite(pts[, 2]), , drop = FALSE]
  if (nrow(pts) < 8L) return(default)
  rx <- (max(pts[, 1]) - min(pts[, 1])) / 2
  ry <- (max(pts[, 2]) - min(pts[, 2])) / 2
  cx <- (max(pts[, 1]) + min(pts[, 1])) / 2
  cy <- (max(pts[, 2]) + min(pts[, 2])) / 2
  if (any(!is.finite(c(cx, cy, rx, ry))) || rx <= 0 || ry <= 0) return(default)
  list(cx = cx, cy = cy, rx = rx, ry = ry)
}

.ggcl_ring_xy <- function(theta, radius_cm, base_cm, circle = NULL,
                            inward = FALSE, inner_frac = 1) {
  circle <- circle %||% list(cx = 0.5, cy = 0.5, rx = 0.5, ry = 0.5)
  off <- grid::unit(base_cm + radius_cm, "cm")
  if (isTRUE(inward)) {
    ## Leaves sit on the inner circle (the hole edge) and the tree grows
    ## towards the centre.
    rxp <- grid::unit(circle$rx * inner_frac, "npc") - off
    ryp <- grid::unit(circle$ry * inner_frac, "npc") - off
  } else {
    rxp <- grid::unit(circle$rx, "npc") + off
    ryp <- grid::unit(circle$ry, "npc") + off
  }
  list(
    x = grid::unit(circle$cx, "npc") + rxp * sin(theta),
    y = grid::unit(circle$cy, "npc") + ryp * cos(theta)
  )
}

## Ring dendrogram grob.
##
## * `rectangle`: radial stubs from each child to the parent radius plus an arc
##   between the two children at the parent radius (the circular counterpart of
##   the usual rectangular dendrogram);
## * `triangle`: a straight chord from each child point to the merge point.
.ggcl_ring_tree_grob <- function(hc, info, spec,
                                 base_cm = 0.55, gap_cm = 0.10,
                                 circle = NULL, inward = FALSE,
                                 inner_frac = 1) {
  n <- length(hc$order)
  if (n < 2L) return(grid::nullGrob())
  height_cm <- as.numeric(spec$height %||% 1)
  if (!is.finite(height_cm) || height_cm <= 0) height_cm <- 1
  if (isTRUE(inward)) {
    ## The hole is only ~inner_radius/0.4 of the circle: clamp the tree so it
    ## cannot spill over the data rings.
    hole_cm <- tryCatch(
      min(
        grid::convertWidth(
          grid::unit((circle$rx %||% 0.5) * inner_frac, "npc"), "cm",
          valueOnly = TRUE
        ),
        grid::convertHeight(
          grid::unit((circle$ry %||% 0.5) * inner_frac, "npc"), "cm",
          valueOnly = TRUE
        )
      ),
      error = function(e) 1.2
    )
    if (!is.finite(hole_cm) || hole_cm <= 0) hole_cm <- 1.2
    height_cm <- min(height_cm, max(0.2, hole_cm - base_cm - 0.15))
  }

  ## Leaf angles in scale order, re-mapped to the tree's observation indices.
  labels <- .ggcl_strip_key(hc$labels %||% as.character(seq_len(n)))
  idx <- match(labels, .ggcl_strip_key(info$labels))
  if (anyNA(idx) || length(idx) != n) return(grid::nullGrob())
  leaf_angle <- info$angles[idx]

  ## Unwrap the leaf angles along the tree's leaf order so that a merge can
  ## average its children without jumping across the 0 / 2*pi seam.
  ord <- hc$order
  unw <- numeric(n)
  unw[1] <- leaf_angle[ord[1]]
  if (n > 1L) {
    for (i in seq_len(n)[-1]) {
      delta <- (leaf_angle[ord[i]] - unw[i - 1L]) %% (2 * pi)
      if (delta > pi) delta <- delta - 2 * pi
      unw[i] <- unw[i - 1L] + delta
    }
  }
  leaf_at <- numeric(n)
  leaf_at[ord] <- unw
  rm(ord)

  heights <- .ggcl_ring_radius_cm(hc$height, height_cm)
  node_angle <- numeric(max(0L, n - 1L))
  node_radius <- heights

  type <- as.character(spec$type %||% "rectangle")[[1]]
  if (!type %in% c("rectangle", "triangle")) type <- "rectangle"
  base_leaf <- 0

  ## Collect all polylines as (theta, radius) pairs with an id per line.
  theta <- numeric(0)
  radius <- numeric(0)
  id <- integer(0)
  add_line <- function(th, rad) {
    theta <<- c(theta, th)
    radius <<- c(radius, rad)
    id <<- c(id, rep.int(length(unique(id)) + 1L, length(th)))
  }
  add_seg <- function(th1, rad1, th2, rad2) {
    add_line(c(th1, th2), c(rad1, rad2))
  }
  add_arc <- function(th1, rad1, th2, rad2) {
    ## An equal-radius branch is an arc after the polar mapping: sample it at
    ## ~1.5 degrees per point (ggtree draws the rectangular layout and lets
    ## `coord_polar()` bend the equal-radius segments; this is the same idea,
    ## with the arc points generated explicitly).
    k <- max(2L, ceiling(abs(th2 - th1) / pi * 120))
    add_line(seq(th1, th2, length.out = k), rep(rad1, k))
  }

  for (i in seq_len(max(0L, n - 1L))) {
    a <- hc$merge[i, 1L]
    b <- hc$merge[i, 2L]
    ta <- if (a < 0L) leaf_at[-a] else node_angle[a]
    tb <- if (b < 0L) leaf_at[-b] else node_angle[b]
    ra <- if (a < 0L) base_leaf else node_radius[a]
    rb <- if (b < 0L) base_leaf else node_radius[b]
    tp <- (ta + tb) / 2
    rp <- node_radius[i]
    node_angle[i] <- tp
    if (identical(type, "triangle")) {
      add_seg(ta, ra, tp, rp)
      add_seg(tb, rb, tp, rp)
    } else {
      add_seg(ta, ra, ta, rp)
      add_seg(tb, rb, tb, rp)
      if (abs(tb - ta) > 1e-9) {
        add_arc(min(ta, tb), rp, max(ta, tb), rp)
      }
    }
  }

  ## Drop degenerate lines (fewer than 2 points after collapsing).
  keep <- rep(TRUE, length(theta))
  xy <- .ggcl_ring_xy(theta, radius, base_cm, circle, inward = inward,
                      inner_frac = inner_frac)
  grid::polylineGrob(
    x = xy$x,
    y = xy$y,
    id = id,
    gp = grid::gpar(
      col = spec$col %||% "black",
      lwd = spec$lwd %||% 0.7,
      lineend = "round"
    )
  )
}

## Radial extent of the theta labels, in cm.
##
## `coord_radial()` draws the labels along the radius, so the tree has to start
## after the longest label rather than right at the circle.  The text width is
## measured with the theme's axis font size; when no device is available the
## fallback keeps a sensible gap.
.ggcl_label_extent_cm <- function(pp, built) {
  labels <- tryCatch(as.character(pp$theta$get_labels()), error = function(e) character(0))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) return(0.55)
  theme <- built$plot$theme
  size <- tryCatch({
    el <- ggplot2::calc_element("axis.text.x.bottom", theme)
    s <- el$size
    if (is.numeric(s)) s else 8.8
  }, error = function(e) 8.8)
  if (!is.finite(size) || size <= 0) size <- 8.8
  w <- tryCatch(
    grid::convertWidth(
      grid::stringWidth(labels, gp = grid::gpar(fontsize = size)),
      "cm", valueOnly = TRUE
    ),
    error = function(e) 0.55
  )
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(0.55)
  max(w) + 0.12
}

## Draw the polar cluster trees into the finished gtable.
##
## The theta tree overflows the panel cell, so a plain overlay would paint over
## the facet strips and over the neighbouring panel.  Instead of asking the user
## to hand-tune `panel.spacing`, a padding band of exactly the tree's radial
## extent is inserted around every panel row / column; the trees then grow
## inside their own band.  The r-axis tree lives in the hole, so it needs no
## padding.
.ggcl_ring_trees_to_gtable <- function(g, built) {
  specs <- built$plot$ggutils_cluster_specs
  if (!length(specs)) return(g)
  x_specs <- Filter(function(s) identical(s$axis %||% "x", "x"), specs)
  y_specs <- Filter(function(s) identical(s$axis %||% "y", "y"), specs)
  if (!length(x_specs) && !length(y_specs)) return(g)

  panel_cells <- function(g) {
    lay <- g$layout
    pl <- which(grepl("^panel($|-[0-9]+-[0-9]+$)", lay$name))
    if (!length(pl)) return(NULL)
    data.frame(
      name = lay$name[pl],
      t = lay$t[pl], b = lay$b[pl], l = lay$l[pl], r = lay$r[pl],
      stringsAsFactors = FALSE
    )
  }

  cells <- panel_cells(g)
  if (is.null(cells)) return(g)
  pp <- built$layout$panel_params
  if (!length(pp)) return(g)

  panel_index <- function(cells, k) {
    meta <- built$layout$layout
    parsed <- regmatches(cells$name[k],
                         regexec("^panel-([0-9]+)-([0-9]+)$", cells$name[k]))[[1]]
    row <- if (length(parsed) >= 3L) as.integer(parsed[3]) else 1L
    col <- if (length(parsed) >= 3L) as.integer(parsed[2]) else 1L
    if (all(c("ROW", "COL") %in% names(meta))) {
      hit <- which(meta$ROW == row & meta$COL == col)
      if (length(hit)) return(hit[1])
    }
    k
  }

  ## -- padding around every panel row / column (outer theta tree only) -------
  if (length(x_specs)) {
    spec_x <- x_specs[[length(x_specs)]]
    ## `position = "bottom"` means "inside the hole", `"top"` stays outside;
    ## only an outside tree needs extra room around the panel.
    inward <- identical(spec_x$position %||% "bottom", "bottom")
    ring_gap <- spec_x$ring_gap
    if (inward) {
      if (is.null(ring_gap)) ring_gap <- 0.08
    } else {
      if (is.null(ring_gap)) ring_gap <- .ggcl_label_extent_cm(pp[[1]], built)
      height_cm <- as.numeric(spec_x$height %||% 1)
      if (!is.finite(height_cm) || height_cm <= 0) height_cm <- 1
      extent <- ring_gap + height_cm + 0.20
      pad <- grid::unit(extent, "cm")
      for (r in sort(unique(cells$t), decreasing = TRUE)) {
        g <- gtable::gtable_add_rows(g, heights = pad, pos = r)
        g <- gtable::gtable_add_rows(g, heights = pad, pos = r - 1L)
      }
      for (cc in sort(unique(cells$l), decreasing = TRUE)) {
        g <- gtable::gtable_add_cols(g, widths = pad, pos = cc)
        g <- gtable::gtable_add_cols(g, widths = pad, pos = cc - 1L)
      }
      cells <- panel_cells(g)
      if (is.null(cells)) return(g)
    }
    if (!is.finite(ring_gap) || ring_gap <= 0) ring_gap <- 0.08

    for (k in seq_len(nrow(cells))) {
      panel <- panel_index(cells, k)
      if (panel < 1L || panel > length(pp)) next
      params <- pp[[panel]]
      info <- .ggcl_theta_break_angles(params)
      if (is.null(info)) next
      hc <- .ggcl_ring_hclust_for_panel(params, spec_x)
      if (is.null(hc)) next
      circle <- .ggcl_theta_circle(params, built$plot$coordinates)
      inner_frac <- suppressWarnings(
        as.numeric(params$inner_radius %||% 0)[1] / 0.4
      )
      if (!is.finite(inner_frac) || inner_frac < 0) inner_frac <- 0
      ## Inside the hole there is nothing to draw without an inner radius.
      if (inward && inner_frac <= 0.02) next
      grob <- tryCatch(
        .ggcl_ring_tree_grob(hc, info, spec_x, base_cm = ring_gap,
                             circle = circle, inward = inward,
                             inner_frac = if (inward) inner_frac else 1),
        error = function(e) grid::nullGrob()
      )
      if (inherits(grob, "null")) next
      g <- gtable::gtable_add_grob(
        g, grob,
        t = cells$t[k], l = cells$l[k], b = cells$b[k], r = cells$r[k],
        clip = "off", name = paste0("ggcl-ring-", panel)
      )
    }
  }

  ## -- r-axis tree: only for partial arcs ------------------------------------
  ## On a full circle `coord_radial()` draws the r axis as a plain column on the
  ## left, where an "arc" tree has no meaning, so nothing is drawn.  Partial
  ## arcs put the r labels on a radial ray, and the tree becomes an arc-shaped
  ## wedge hanging off that ray (see `.ggcl_r_wedge_tree_grob()`).
  if (length(y_specs)) {
    spec_y <- y_specs[[length(y_specs)]]
    height_cm <- as.numeric(spec_y$height %||% 1)
    if (!is.finite(height_cm) || height_cm <= 0) height_cm <- 1.2
    coord <- built$plot$coordinates
    if (!isTRUE(coord$r_axis_inside)) return(g)

    ## The tree grows outside the arc; reserve room so it is not clipped.
    cells <- panel_cells(g)
    if (is.null(cells)) return(g)
    pad <- grid::unit(height_cm + .ggcl_label_extent_cm(pp[[1]], built) + 0.4, "cm")
    for (rr in sort(unique(cells$t), decreasing = TRUE)) {
      g <- gtable::gtable_add_rows(g, heights = pad, pos = rr)
      g <- gtable::gtable_add_rows(g, heights = pad, pos = rr - 1L)
    }
    for (cc in sort(unique(cells$l), decreasing = TRUE)) {
      g <- gtable::gtable_add_cols(g, widths = pad, pos = cc)
      g <- gtable::gtable_add_cols(g, widths = pad, pos = cc - 1L)
    }
    cells <- panel_cells(g)
    if (is.null(cells)) return(g)
    for (k in seq_len(nrow(cells))) {
      panel <- panel_index(cells, k)
      if (panel < 1L || panel > length(pp)) next
      params <- pp[[panel]]
      info <- .ggcl_r_break_positions(params)
      if (is.null(info)) next
      hc <- .ggcl_r_hclust_for_panel(params, spec_y)
      if (is.null(hc)) next
      circle <- .ggcl_theta_circle(params, built$plot$coordinates)
      rot <- params$axis_rotation
      if (!is.numeric(rot) || length(rot) < 2L) next
      inner_frac <- suppressWarnings(
        as.numeric(params$inner_radius %||% 0)[1] / 0.4
      )
      if (!is.finite(inner_frac) || inner_frac < 0) inner_frac <- 0
      ## ggplot2 draws the r labels at the *start* of the arc, so the tree goes
      ## to the other end (the side without labels) and grows outwards.
      grob <- tryCatch(
        .ggcl_r_wedge_tree_grob(
          hc, info, spec_y, circle, theta0 = rot[2],
          angle_sign = 1,
          angle_span = min(pi / 3, 0.30 * height_cm),
          inner_frac = inner_frac,
          base_angle = 0.01 + 0.002 * length(hc$order)
        ),
        error = function(e) grid::nullGrob()
      )
      if (inherits(grob, "null")) next
      g <- gtable::gtable_add_grob(
        g, grob,
        t = cells$t[k], l = cells$l[k], b = cells$b[k], r = cells$r[k],
        clip = "off", name = paste0("ggcl-rtree-", panel)
      )
    }
  }
  g
}
## ---------------------------------------------------------------------------
## Radial (r axis) dendrogram: leaves lie on the radius, merges grow sideways
## ---------------------------------------------------------------------------
## The r axis of `coord_radial()` has no angle dimension: the categories sit at
## different radii along one ray.  Its tree is therefore drawn as a *linear*
## dendrogram in the hole/inner region: leaves on that ray, each merge offset
## tangentially (an angular displacement) by its height.
.ggcl_r_break_positions <- function(pp) {
  view <- pp$r
  if (is.null(view)) return(NULL)
  breaks <- as.character(view$get_breaks())
  if (length(breaks) < 2L) return(NULL)
  cr <- view$continuous_range
  if (is.null(cr) || length(cr) != 2L || !all(is.finite(cr)) || diff(cr) == 0) {
    cr <- c(0.5, length(breaks) + 0.5)
  }
  list(labels = breaks, frac = (seq_along(breaks) - cr[1]) / diff(cr))
}

.ggcl_r_hclust_for_panel <- function(pp, spec) {
  view <- pp$r
  if (is.null(view)) return(NULL)
  breaks <- as.character(view$get_breaks())
  hc <- NULL
  if (isTRUE(spec$free)) {
    parts <- strsplit(breaks[1], .ggcl_sep, fixed = TRUE)[[1]]
    key <- paste(parts[-length(parts)], collapse = .ggcl_sep)
    hc <- spec$panel_hclust[[key]]
  }
  if (is.null(hc) || !inherits(hc, "hclust")) hc <- spec$hclust
  if ((is.null(hc) || !inherits(hc, "hclust")) && !is.null(spec$mat)) {
    idx <- match(.ggcl_strip_key(breaks), rownames(spec$mat))
    if (!anyNA(idx) && length(idx) >= 2L) {
      hc <- stats::hclust(stats::dist(spec$mat[idx, , drop = FALSE]),
                          method = spec$method %||% "complete")
      hc$labels <- rownames(spec$mat)[idx]
    }
  }
  if (is.null(hc) || !inherits(hc, "hclust")) return(NULL)
  hc
}

## A linear dendrogram in (r, tangential-offset) coordinates.
.ggcl_r_tree_grob <- function(hc, info, spec, circle, inner_frac,
                              axis_rotation = 0, height_cm = 1) {
  n <- length(hc$order)
  if (n < 2L) return(grid::nullGrob())
  labels <- .ggcl_strip_key(hc$labels %||% as.character(seq_len(n)))
  idx <- match(labels, .ggcl_strip_key(info$labels))
  if (anyNA(idx) || length(idx) != n) return(grid::nullGrob())

  ## The tree lives entirely inside the hole: leaves are spread between the
  ## centre and the hole edge (matching the category order, not their data
  ## radii, which lie outside the hole under the heat map rings).
  leaf_frac <- inner_frac * (0.14 + 0.72 * info$frac[idx])
  ord <- hc$order
  leaf_at <- numeric(n)
  leaf_at[ord] <- leaf_frac

  heights <- hc$height
  hmax <- max(c(heights, 1e-8))
  h <- heights / hmax
  ## Tangential displacement: ~0.10 rad per cm, clamped so even a tall tree
  ## stays inside the hole.
  amax <- min(1.25, height_cm * 0.45)
  node_r <- h + 0
  node_frac <- numeric(max(0L, n - 1L))
  for (i in seq_len(max(0L, n - 1L))) {
    a <- hc$merge[i, 1L]
    b <- hc$merge[i, 2L]
    fa <- if (a < 0L) leaf_at[-a] else node_frac[a]
    fb <- if (b < 0L) leaf_at[-b] else node_frac[b]
    node_frac[i] <- (fa + fb) / 2
  }

  theta <- numeric(0); ratio <- numeric(0); id <- integer(0)
  add <- function(th, rf) {
    theta <<- c(theta, th)
    ratio <<- c(ratio, rf)
    id <<- c(id, rep.int(length(unique(id)) + 1L, length(th)))
  }
  for (i in seq_len(max(0L, n - 1L))) {
    a <- hc$merge[i, 1L]; b <- hc$merge[i, 2L]
    fa <- if (a < 0L) leaf_at[-a] else node_frac[a]
    fb <- if (b < 0L) leaf_at[-b] else node_frac[b]
    dr <- h[i] * amax
    da <- if (a < 0L) 0 else h[a] * amax
    db <- if (b < 0L) 0 else h[b] * amax
    ## tangential segments (constant radius, growing offset)
    add(c(axis_rotation + da, axis_rotation + dr), c(fa, fa))
    add(c(axis_rotation + db, axis_rotation + dr), c(fb, fb))
    ## radial segment at the parent offset
    add(rep(axis_rotation + dr, 2), c(fa, fb))
  }
  ## Drop duplicated points and convert to npc.
  x <- grid::unit(circle$cx, "npc") +
    (grid::unit(circle$rx, "npc") * ratio) * sin(theta)
  y <- grid::unit(circle$cy, "npc") +
    (grid::unit(circle$ry, "npc") * ratio) * cos(theta)
  grid::polylineGrob(x = x, y = y, id = id,
                     gp = grid::gpar(col = spec$col %||% "black",
                                     lwd = spec$lwd %||% 0.7,
                                     lineend = "round"))
}




## ---------------------------------------------------------------------------
## Y-axis tree for partial arcs: arc-shaped wedge dendrogram
## ---------------------------------------------------------------------------
## `arcHeatmapGrob()` draws the r-axis (row) tree like this: the standard
## dendrogram's leaf position is mapped onto the *radius* and its merge height
## onto a small *angle* wedge placed beside the arc.  Every straight branch of
## the standard tree therefore becomes a concentric arc, and the branch that
## joins two children becomes a radial line -- the tree hugs the circle.
## Angular offset (radians) needed to clear the r-axis label text.
##
## The labels are written along the radial ray, so a label at radius `r` covers
## about `width / r` radians.  Take the widest one (relative to its radius) and
## add a small margin; the wedge tree then starts after the text instead of on
## top of it.
.ggcl_r_label_angle <- function(pp, built, frac) {
  labels <- tryCatch(as.character(pp$r$get_labels()), error = function(e) character(0))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels) || !length(frac)) return(0.30)
  theme <- built$plot$theme
  size <- tryCatch({
    el <- ggplot2::calc_element("axis.text.y", theme)
    s <- el$size
    if (is.numeric(s)) s else 8.8
  }, error = function(e) 8.8)
  if (!is.finite(size) || size <= 0) size <- 8.8
  widths <- tryCatch(
    grid::convertWidth(
      grid::stringWidth(labels, gp = grid::gpar(fontsize = size)),
      "cm", valueOnly = TRUE
    ),
    error = function(e) rep(0.7, length(labels))
  )
  ## Circle radius in cm, estimated from the device (the grob is built before
  ## the panel viewport exists, so npc cannot be converted here).
  din <- tryCatch(graphics::par("din"), error = function(e) c(7, 5))
  r_cm <- max(1.2, min(din) * 2.54 * 0.24)
  ## Worst case: the innermost label with the widest text.
  leaf_r <- pmax(frac, 0.08)
  ang <- widths / (r_cm * leaf_r)
  ang <- ang[is.finite(ang)]
  if (!length(ang)) return(0.30)
  min(pi / 2, max(ang) + 0.10)
}

.ggcl_r_wedge_tree_grob <- function(hc, info, spec, circle, theta0,
                                    angle_sign = -1, angle_span = pi / 6,
                                    inner_frac = 0, base_angle = 0.28) {
  n <- length(hc$order)
  if (n < 2L) return(grid::nullGrob())
  labels <- .ggcl_strip_key(hc$labels %||% as.character(seq_len(n)))
  idx <- match(labels, .ggcl_strip_key(info$labels))
  if (anyNA(idx) || length(idx) != n) return(grid::nullGrob())

  ## leaf position -> radius (the r axis position of every sample)
  leaf_r <- inner_frac + info$frac[idx] * (1 - inner_frac)

  heights <- hc$height
  hmax <- max(c(heights, 1e-8))
  h <- heights / hmax
  node_r <- numeric(n - 1L)
  for (i in seq_len(n - 1L)) {
    a <- hc$merge[i, 1L]; b <- hc$merge[i, 2L]
    ra <- if (a < 0L) leaf_r[-a] else node_r[a]
    rb <- if (b < 0L) leaf_r[-b] else node_r[b]
    node_r[i] <- (ra + rb) / 2
  }

  rx <- circle$rx %||% 0.5
  ry <- circle$ry %||% 0.5
  pt <- function(r, ang) {
    list(
      x = grid::unit(circle$cx, "npc") + grid::unit(rx * r, "npc") * sin(ang),
      y = grid::unit(circle$cy, "npc") + grid::unit(ry * r, "npc") * cos(ang)
    )
  }
  ang_of <- function(hh) theta0 + angle_sign * (base_angle + hh * angle_span)

  segs <- list()
  push_seg <- function(r1, a1, r2, a2, k = 2L) {
    tt <- seq(0, 1, length.out = max(2L, k))
    rr <- r1 + (r2 - r1) * tt
    aa <- a1 + (a2 - a1) * tt
    x <- grid::unit(circle$cx, "npc") + grid::unit(rx * rr, "npc") * sin(aa)
    y <- grid::unit(circle$cy, "npc") + grid::unit(ry * rr, "npc") * cos(aa)
    segs[[length(segs) + 1L]] <<- list(x = x, y = y)
  }
  for (i in seq_len(n - 1L)) {
    a <- hc$merge[i, 1L]; b <- hc$merge[i, 2L]
    ra <- if (a < 0L) leaf_r[-a] else node_r[a]
    rb <- if (b < 0L) leaf_r[-b] else node_r[b]
    ha <- if (a < 0L) 0 else h[a]
    hb <- if (b < 0L) 0 else h[b]
    hp <- h[i]
    ## child stubs: constant radius -> arcs along the wedge
    push_seg(ra, ang_of(ha), ra, ang_of(hp),
             k = max(6L, ceiling(abs(ang_of(hp) - ang_of(ha)) * 40)))
    push_seg(rb, ang_of(hb), rb, ang_of(hp),
             k = max(6L, ceiling(abs(ang_of(hp) - ang_of(hb)) * 40)))
    ## the branch joining the two children: constant angle -> radial line
    push_seg(ra, ang_of(hp), rb, ang_of(hp), k = 2L)
  }
  xs <- do.call(grid::unit.c, lapply(segs, `[[`, "x"))
  ys <- do.call(grid::unit.c, lapply(segs, `[[`, "y"))
  ids <- rep(seq_along(segs),
             times = vapply(segs, function(s) length(s$x), integer(1)))
  grid::polylineGrob(x = xs, y = ys, id = ids,
                     gp = grid::gpar(col = spec$col %||% "black",
                                     lwd = spec$lwd %||% 0.7,
                                     lineend = "round"))
}

## ---------------------------------------------------------------------------
## y-axis (r) dendrogram, mapped like `arcHeatmapGrob()`'s column tree
## ---------------------------------------------------------------------------
## `arcHeatmapGrob()` turns a rectangular dendrogram into an arc tree by mapping
## the leaf order to an angle and the merge height to a radius, then drawing the
## segments with `arcSegmentsGrob()`: a horizontal branch becomes a concentric
## arc, a leaf stub becomes a radial line.  We do the same here for the y
## (sample) axis:
##
##   leaf order -> angle (spread over an angular span)
##   merge height -> radius (band outside the circle, or inside the hole)
##
## `position = "top"`     : band outside the outer circle, full arc
## `position = "left"` / `"right"` : outside band restricted to that side
## `position = "bottom"`  : band inside the hole (needs `inner.radius > 0`)
.ggcl_r_band_tree_grob <- function(hc, spec, circle, position = "top",
                                   height_cm = 1, arc = c(0, 2 * pi),
                                   inner_frac = 0, base_cm = 0.10) {
  n <- length(hc$order)
  if (n < 2L) return(grid::nullGrob())
  position <- position %||% "top"
  position <- as.character(position)[1]
  inward <- identical(position, "bottom")

  ## Angular span of the leaf band.
  if (position %in% c("left", "right")) {
    center <- if (identical(position, "right")) pi / 2 else -pi / 2
    span <- min(pi, max(0.4, n * 0.22))
  } else {
    span <- diff(range(arc))
    if (!is.finite(span) || span <= 0) span <- 2 * pi
    center <- mean(range(arc))
  }
  margin <- min(span * 0.08, 0.15)
  ang_order <- seq(center - span / 2 + margin, center + span / 2 - margin,
                   length.out = n)
  leaf_angles <- numeric(n)
  leaf_angles[hc$order] <- ang_order
  info <- list(labels = as.character(hc$labels %||% seq_len(n)),
               angles = leaf_angles)
  spec2 <- spec
  spec2$height <- height_cm
  ## The band sits outside the circle (outward) or inside the hole (inward);
  ## both reuse the ring-dendrogram geometry of the theta tree.
  .ggcl_ring_tree_grob(
    hc, info, spec2,
    base_cm = if (inward) 0.05 else base_cm,
    circle = circle,
    inward = inward,
    inner_frac = max(inner_frac, 0.02)
  )
}

## ---------------------------------------------------------------------------
## Outside r-axis tree: a vertical dendrogram next to the r labels, with the
## along-axis branches drawn as arcs so it reads as part of the circular plot.
## ---------------------------------------------------------------------------
## Leaves are placed at the y positions of the r-axis labels (converted to npc),
## the tree grows away from the panel (left for `position = "left"`, right for
## `"right"`), and every branch that runs along the axis is drawn as an arc
## instead of a straight line.
.ggcl_r_outer_tree_grob <- function(hc, info, spec, height_cm = 1.2,
                                    side = "left") {
  n <- length(hc$order)
  if (n < 2L) return(grid::nullGrob())
  labels <- .ggcl_strip_key(hc$labels %||% as.character(seq_len(n)))
  idx <- match(labels, .ggcl_strip_key(info$labels))
  if (anyNA(idx) || length(idx) != n) return(grid::nullGrob())

  ## r-axis labels are laid out along the panel, in data order; use the
  ## "continuous position -> npc" fraction of the r scale.
  frac <- info$frac[idx]
  ## Vertical npc; ggplot2 draws this axis top-down for the r scale, so put the
  ## first beak at the top.
  y_leaf <- 1 - frac
  ord <- hc$order
  leaf_at <- numeric(n)
  leaf_at[ord] <- y_leaf

  heights <- .ggcl_ring_radius_cm(hc$height, 1)
  node_y <- numeric(max(0L, n - 1L))
  for (i in seq_len(max(0L, n - 1L))) {
    a <- hc$merge[i, 1L]
    b <- hc$merge[i, 2L]
    ya <- if (a < 0L) leaf_at[-a] else node_y[a]
    yb <- if (b < 0L) leaf_at[-b] else node_y[b]
    node_y[i] <- (ya + yb) / 2
  }
  ## Depth fraction 0 = leaves (panel side), 1 = root (outside).
  depth <- heights / max(c(heights, 1e-8))
  grow <- if (identical(side, "right")) 1 else -1
  x_of <- function(d) {
    if (grow < 0) 1 - d * 0.85 else 0 + d * 0.85
  }

  add_arc <- function(y1, y2, xp, bulge) {
    ## arc along the axis, bulging away from the panel
    k <- max(8L, ceiling(abs(y2 - y1) * 60))
    tt <- seq(0, 1, length.out = k)
    y <- y1 + (y2 - y1) * tt
    x <- xp + bulge * sin(pi * tt)
    list(x = x, y = y)
  }

  xs <- numeric(0); ys <- numeric(0); id <- integer(0)
  add <- function(x, y) {
    xs <<- c(xs, x); ys <<- c(ys, y)
    id <<- c(id, rep.int(length(unique(id)) + 1L, length(x)))
  }
  for (i in seq_len(max(0L, n - 1L))) {
    a <- hc$merge[i, 1L]; b <- hc$merge[i, 2L]
    ya <- if (a < 0L) leaf_at[-a] else node_y[a]
    yb <- if (b < 0L) leaf_at[-b] else node_y[b]
    xa <- if (a < 0L) x_of(0) else x_of(depth[a])
    xb <- if (b < 0L) x_of(0) else x_of(depth[b])
    xp <- x_of(depth[i])
    ## straight (radial) branches from each child to the parent depth
    add(c(xa, xp), c(ya, ya))
    add(c(xb, xp), c(yb, yb))
    ## ordinary dendrogram: the branch along the axis is a straight segment
    add(c(xp, xp), c(ya, yb))
  }
  grid::polylineGrob(x = grid::unit(xs, "npc"), y = grid::unit(ys, "npc"),
                     id = id,
                     gp = grid::gpar(col = spec$col %||% "black",
                                     lwd = spec$lwd %||% 0.7,
                                     lineend = "round"))
}
