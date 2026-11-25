## ---------------------------------------------------------------------------
## Theta (radial) axis: label layout
## ---------------------------------------------------------------------------
## `coord_radial()` puts the labels on a ring around the panel.  Following
## `circlize::circos.labels()`, the labels are always rotated along the radius
## so that the text points towards the centre of the circle, and each label is
## connected to its tick with the three-segment leader line
## ("stub - diagonal - stub").
##
## * outside (default): labels sit on a label track outside the circle;
## * inside: labels sit inside the circle, attached to the inner radius
##   (`coord_radial(inner.radius =)`), e.g. in the hole of a circular heatmap.
##
## Crowded labels are spread along the arc with `smartAlign`, the same
## algorithm circlize uses in `posTransform.text()`.

## Spread the labels along the arc with `smartAlign`, exactly like circlize's
## `posTransform.text()`.  The circle is cut at its largest gap so the
## arrangement never crosses the 0 / 1 seam.
.ggse_theta_spread <- function(pos, widths, wrap = TRUE) {
  n <- length(pos)
  if (n == 0L) {
    return(list(pos = numeric(), shown = logical()))
  }
  ord <- order(pos)
  p <- pos[ord]
  w <- widths[ord]
  shift <- 0
  if (isTRUE(wrap) && n > 1L) {
    gaps <- diff(c(p, p[1] + 1))
    k <- which.max(gaps)
    shift <- p[k] + gaps[k] / 2
    p <- (p - shift) %% 1
    ord <- order(p)
    p <- p[ord]
    w <- w[ord]
  }
  starts <- pmax(p - w / 2, 0)
  ends <- pmin(p + w / 2, 1)
  adj <- smart_align(starts, ends, range = c(0, 1))
  starts <- adj$start
  ends <- adj$end

  shown_ord <- starts >= -1e-9 & ends <= 1 + 1e-9
  spread_ord <- (starts + ends) / 2
  if (isTRUE(wrap) && n > 1L) {
    spread_ord <- (spread_ord + shift) %% 1
  }
  spread <- numeric(n)
  spread[ord] <- spread_ord
  out <- logical(n)
  out[ord] <- shown_ord
  list(pos = spread, shown = out)
}

## Rotation (degrees) of a label: text runs along the radius and points towards
## the centre of the circle.  Labels on the left half are turned around so they
## are never upside down; `flip` tells the drawing code which end of the text
## touches its anchor.
.ggse_theta_rot <- function(theta, adjust = 0) {
  rot <- 90 - theta * 180 / pi + adjust
  rot <- (rot + 180) %% 360 - 180
  flip <- rot > 90 | rot < -90
  list(angle = ifelse(flip, rot + 180, rot), flip = flip)
}

## Build the label grob for a theta axis (circlize-style label track).
.ggse_theta_label_grob <- function(key, elements, params) {
  labels <- as.character(key$.label)
  if (length(labels) == 0L) {
    return(ggplot2::zeroGrob())
  }
  style <- .ggse_text_style(elements$text)
  if (is.null(style)) {
    return(ggplot2::zeroGrob())
  }

  theta_side <- params$theta_side %||% "outside"
  if (!theta_side %in% c("outside", "inside")) {
    theta_side <- "outside"
  }
  inside <- identical(theta_side, "inside")

  theta <- key$theta
  dev_cm <- .ggse_device_size_cm()
  ## the key points sit on the outer ring; `radius_cm` estimates that radius
  radius_cm <- max(1, params$panel_width %||% (0.30 * min(dev_cm)))
  box <- .ggse_text_box_cm(
    labels, size = style$size, colour = style$colour, family = style$family,
    face = style$face, lineheight = style$lineheight, angle = 0
  )
  widths_cm <- box$width
  heights_cm <- pmax(box$height, 0.4 * .ggse_line_height_cm(style$size, style$lineheight))

  ## `coord_radial(start =, end =)` may draw only an arc; normalise angles to
  ## the drawn arc so gaps are measured in arc length.
  arc <- params$smart_arc %||% c(0, 2 * pi)
  span <- diff(arc)
  if (!is.finite(span) || span <= 0) {
    span <- 2 * pi
    arc <- c(0, 2 * pi)
  }
  pos <- ((theta - arc[1]) %% (2 * pi)) / span
  pos <- pmin(pmax(pos, 0), 1)

  ## `coord_radial()` stores the hole as `c(inner.radius, 1) * 0.4` in
  ## `panel_params$inner_radius`, so the actual fraction of the radius is
  ## `inner_radius[1] / 0.4` (0 for a full pie)
  inner_frac <- suppressWarnings(as.numeric(params$smart_inner %||% 0)[1] / 0.4)
  if (!is.finite(inner_frac) || inner_frac < 0) inner_frac <- 0
  inner_frac <- min(inner_frac, 1)
  radius_cm_label <- if (inside && inner_frac > 0) {
    max(0.5, inner_frac * radius_cm)
  } else {
    radius_cm
  }

  ## the text is radial, so its tangential footprint is the line height.  Inside
  ## labels grow towards the centre, where the circle is smaller: measure the
  ## footprint at the inner end of the text so that neighbours do not collide.
  radius_foot <- if (inside) {
    max(0.5, radius_cm_label - max(widths_cm))
  } else {
    radius_cm_label
  }
  widths_arc <- heights_cm / radius_foot / span

  ## spread the labels along the arc so that they do not overlap
  wrap <- span > 1.8 * pi
  spread <- .ggse_theta_spread(pos, widths_arc, wrap = wrap)
  keep <- spread$shown
  if (!any(keep)) {
    return(ggplot2::zeroGrob())
  }
  if (any(!keep)) {
    .ggse_inform(
      "Only {sum(keep)} of {length(keep)} labels fit around the ring; {sum(!keep)} were dropped. Use {.arg which} / {.arg n} or a larger device."
    )
  }
  labels <- labels[keep]
  theta <- theta[keep]
  widths_cm <- widths_cm[keep]
  heights_cm <- heights_cm[keep]
  keep_idx <- which(keep)
  spread_pos <- spread$pos[keep]

  ## height of the connection band (circlize's `connection_height`, 5 mm)
  offset_cm <- tryCatch(
    grid::convertWidth(elements$offset, "cm", valueOnly = TRUE),
    error = function(e) 0
  )
  if (!is.finite(offset_cm) || offset_cm < 0) offset_cm <- 0
  conn_cm <- max(.ggse_mm(5), offset_cm)
  stub_cm <- conn_cm / 3

  theta_lab <- arc[1] + spread_pos * span
  adjust <- if (is.numeric(params$angle)) params$angle else 0
  rot <- .ggse_theta_rot(theta_lab, adjust)

  r_npc <- sqrt((key$x[keep_idx] - 0.5)^2 + (key$y[keep_idx] - 0.5)^2)
  if (inside && inner_frac > 0) {
    ## label track attached to the inner ring, text pointing at the centre
    boundary_npc <- r_npc * inner_frac
    depth_cm <- conn_cm
    x_lab <- grid::unit(0.5, "npc") +
      (grid::unit(boundary_npc, "npc") - grid::unit(depth_cm, "cm")) * sin(theta_lab)
    y_lab <- grid::unit(0.5, "npc") +
      (grid::unit(boundary_npc, "npc") - grid::unit(depth_cm, "cm")) * cos(theta_lab)
    ## three-segment leader, exactly like circlize: a radial stub starting on
    ## the inner circle, a diagonal, and a radial stub at the label
    p0x <- grid::unit(0.5, "npc") + grid::unit(boundary_npc, "npc") * sin(theta)
    p0y <- grid::unit(0.5, "npc") + grid::unit(boundary_npc, "npc") * cos(theta)
    p1x <- p0x - grid::unit(stub_cm, "cm") * sin(theta)
    p1y <- p0y - grid::unit(stub_cm, "cm") * cos(theta)
    p2x <- grid::unit(0.5, "npc") +
      (grid::unit(boundary_npc, "npc") - grid::unit(depth_cm - stub_cm, "cm")) * sin(theta_lab)
    p2y <- grid::unit(0.5, "npc") +
      (grid::unit(boundary_npc, "npc") - grid::unit(depth_cm - stub_cm, "cm")) * cos(theta_lab)
    ## text grows towards the centre
    hjust <- ifelse(rot$flip, 0, 1)
  } else {
    x_lab <- grid::unit(0.5, "npc") +
      (grid::unit(r_npc, "npc") + grid::unit(conn_cm, "cm")) * sin(theta_lab)
    y_lab <- grid::unit(0.5, "npc") +
      (grid::unit(r_npc, "npc") + grid::unit(conn_cm, "cm")) * cos(theta_lab)
    p0x <- grid::unit(key$x[keep_idx], "npc")
    p0y <- grid::unit(key$y[keep_idx], "npc")
    p1x <- p0x + grid::unit(stub_cm, "cm") * sin(theta)
    p1y <- p0y + grid::unit(stub_cm, "cm") * cos(theta)
    p2x <- grid::unit(0.5, "npc") +
      (grid::unit(r_npc, "npc") + grid::unit(2 * stub_cm, "cm")) * sin(theta_lab)
    p2y <- grid::unit(0.5, "npc") +
      (grid::unit(r_npc, "npc") + grid::unit(2 * stub_cm, "cm")) * cos(theta_lab)
    ## text grows outwards
    hjust <- ifelse(rot$flip, 1, 0)
  }

  text <- ggplot2::element_grob(
    elements$text,
    label = labels,
    x     = x_lab,
    y     = y_lab,
    hjust = hjust,
    vjust = 0.5,
    angle = rot$angle
  )

  connection <- params$connection %||% TRUE
  if (!isTRUE(connection)) {
    return(grid::grobTree(text))
  }
  lines <- grid::segmentsGrob(
    x0 = grid::unit.c(p0x, p1x, p2x),
    y0 = grid::unit.c(p0y, p1y, p2y),
    x1 = grid::unit.c(p1x, p2x, x_lab),
    y1 = grid::unit.c(p1y, p2y, y_lab),
    gp = grid::gpar(
      col = params$connection_col %||% style$colour,
      lwd = params$connection_lwd %||% 0.5,
      lty = params$connection_lty %||% 1
    )
  )
  grid::grobTree(text, lines)
}
