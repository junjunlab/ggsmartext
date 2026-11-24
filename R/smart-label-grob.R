## Construction of the label stack grob and its draw-time layout.
##
## Why a custom grob with `makeContent()`?
## The guide is asked for its size *before* the plot is drawn, at a moment when
## the width of the panel is still unknown (ggplot2 uses relative units for the
## panel).  The exact label layout, however, depends on that width.  So the grob
## reserves a number of tracks at build time and computes the exact positions
## only in `makeContent()`, where the real viewport size is available.

## Build the label grob from `.ggse_prepare()` output.
.ggse_label_grob <- function(prep, params) {
  shown <- prep$shown
  g <- grid::gTree(
    labels       = prep$labels[shown],
    pos          = prep$pos[shown],
    box_along    = prep$box_along_cm[shown],
    box_across   = prep$box_across_cm[shown],
    nrow         = prep$nrow,
    row_h        = prep$row_h_cm,
    link_cm      = prep$link_cm,
    vertical     = prep$vertical,
    position     = prep$position,
    style        = prep$style,
    connection   = prep$connection,
    connection_style = prep$connection_style,
    connection_col   = prep$connection_col,
    connection_lwd   = prep$connection_lwd,
    connection_lty   = prep$connection_lty,
    extend       = prep$extend,
    pad_cm       = prep$pad_cm,
    hjust        = prep$hjust,
    vjust        = prep$vjust,
    name         = "ggsmartext.labels",
    cl           = "ggse_smart_labels"
  )
  attr(g, "ggse_size_cm") <- prep$total_across_cm
  g
}

## Direction of the "outside" of the axis, expressed in the coordinate system of
## the axis gtable cell: `near` is the edge that touches the ticks, `outward`
## is the direction in which the labels are stacked.
.ggse_axis_geometry <- function(position) {
  switch(position,
    bottom = list(near = 1, outward = -1),
    top    = list(near = 0, outward = 1),
    left   = list(near = 1, outward = -1),
    right  = list(near = 0, outward = 1),
    list(near = 1, outward = -1)
  )
}

## grid's S3 method for the custom grob class; registered in .onLoad().
makeContent.ggse_smart_labels <- function(x) {
  style <- x$style
  n_labels <- length(x$labels)
  if (is.null(style) || n_labels == 0L || x$nrow < 1L) {
    return(grid::setChildren(x, grid::gList()))
  }

  vertical <- isTRUE(x$vertical)

  ## Size of the current viewport.  `along` is the direction of the axis,
  ## `across` is the direction in which the tracks are stacked.
  along_cm <- if (vertical) {
    grid::convertHeight(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
  } else {
    grid::convertWidth(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
  }
  across_cm <- if (vertical) {
    grid::convertWidth(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
  } else {
    grid::convertHeight(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
  }
  if (!is.finite(along_cm) || along_cm <= 0) {
    along_cm <- 14
  }
  if (!is.finite(across_cm) || across_cm <= 0) {
    across_cm <- x$nrow * x$row_h
  }
  if (!is.finite(across_cm) || across_cm <= 0) {
    across_cm <- 2
  }

  ## Sizes normalised to the viewport.
  box_along_npc <- x$box_along / along_cm
  box_across_npc <- x$box_across / across_cm
  pad_npc <- x$pad_cm / along_cm
  nrow <- as.integer(x$nrow)

  ## -- exact layout -------------------------------------------------------
  ## The overlap-free arrangement is computed in npc units of the axis, but the
  ## *across* direction is ignored here: `smart_stack()` works on positions
  ## along the axis only.
  lay <- smart_stack(
    positions = x$pos,
    widths    = box_along_npc,
    n         = nrow,
    range     = c(0, 1) + c(-x$extend, x$extend),
    padding   = pad_npc,
    priority  = NULL
  )
  lay <- lay[order(lay$index), , drop = FALSE]
  keep <- as.logical(lay$shown)
  if (!any(keep)) {
    return(grid::setChildren(x, grid::gList()))
  }

  if (isTRUE(getOption("ggsmartext.debug"))) {
    message(
      "ggsmartext makeContent: vertical=", vertical,
      " along_cm=", .ggse_fmt(along_cm),
      " across_cm=", .ggse_fmt(across_cm),
      " nrow=", nrow,
      " connection=", isTRUE(x$connection),
      " conn_col=", paste(x$connection_col, collapse = ","),
      " kept=", sum(keep)
    )
  }

  geo <- .ggse_axis_geometry(x$position)
  near <- geo$near
  outward <- geo$outward

  rows <- lay$row[keep]
  center <- lay$center[keep]
  a0 <- x$pos[keep]

  ## `link_npc`: distance between the axis and the first track of labels.
  ## `row_h_npc`: width of one track.
  link_cm <- x$link_cm
  if (is.null(link_cm) || !is.finite(link_cm) || link_cm < 0) link_cm <- 0
  link_npc <- link_cm / across_cm
  row_h_npc <- x$row_h / across_cm

  ## Across coordinate at which the text of every label starts.  This mirrors
  ## `ComplexHeatmap::anno_mark()`: the label sits at the outer end of the
  ## leader line and grows away from the axis.
  anchor_across <- near + outward * (link_npc + (rows - 1L) * row_h_npc)


  ## -- anchors of the text -------------------------------------------------
  angle <- style$angle
  if (is.null(angle) || !is.finite(angle)) angle <- 0
  centred_rotation <- abs(angle %% 360) > 1e-8 && abs(angle %% 360 - 360) > 1e-8
  if (!is.null(x$hjust) || !is.null(x$vjust)) {
    hjust <- x$hjust %||% 0.5
    vjust <- x$vjust %||% 0.5
  } else if (vertical) {
    hjust <- 0.5 * (1 - outward)
    vjust <- 0.5
  } else {
    hjust <- 0.5
    vjust <- 0.5 * (1 - outward)
  }
  if (centred_rotation && is.null(x$hjust) && is.null(x$vjust)) {
    ## Rotated labels are centred inside their track.
    hjust <- 0.5
    vjust <- 0.5
    anchor_across <- anchor_across + outward * row_h_npc / 2
  }
  anchor_along <- center

  gp_text <- grid::gpar(
    fontsize   = style$size,
    col        = style$colour,
    fontfamily = style$family,
    fontface   = style$face,
    lineheight = style$lineheight
  )

  to_xy <- function(along, across) {
    if (vertical) {
      list(x = grid::unit(across, "npc"), y = grid::unit(along, "npc"))
    } else {
      list(x = grid::unit(along, "npc"), y = grid::unit(across, "npc"))
    }
  }

  text_grobs <- lapply(seq_along(rows), function(i) {
    xy <- to_xy(anchor_along[i], anchor_across[i])
    grid::textGrob(
      label = x$labels[keep][i],
      x     = xy$x,
      y     = xy$y,
      rot   = angle,
      hjust = hjust,
      vjust = vjust,
      gp    = gp_text
    )
  })

  ## -- leader lines --------------------------------------------------------
  ## Geometry follows `ComplexHeatmap::anno_mark()`:
  ##   axis --(perpendicular stub)--> --(diagonal)--> --(perpendicular stub)--> text
  ## i.e. the first and the last third of the link band run perpendicular to the
  ## axis, the middle third is the diagonal that absorbs the shift of the label.
  conn_grob <- NULL
  if (isTRUE(x$connection)) {
    style_conn <- x$connection_style %||% "diagonal"
    gp_conn <- grid::gpar(
      col     = x$connection_col,
      lwd     = x$connection_lwd,
      lty     = x$connection_lty,
      lineend = "round"
    )

    ## Distance from the axis to the text anchor, minus the small gap before
    ## the text itself.
    gap_text <- .ggse_mm(1) / across_cm
    d_text <- link_npc + (rows - 1L) * row_h_npc
    d_seg <- pmax(d_text - gap_text, 0)
    d1 <- d_seg / 3
    d2 <- d_seg * (2 / 3)

    a_start <- c()
    c_start <- c()
    a_end <- c()
    c_end <- c()

    add_seg <- function(a0_, c0_, a1_, c1_) {
      a_start <<- c(a_start, a0_)
      c_start <<- c(c_start, c0_)
      a_end <<- c(a_end, a1_)
      c_end <<- c(c_end, c1_)
    }

    for (i in seq_along(rows)) {
      ai <- a0[i]
      hi <- center[i]
      if (identical(style_conn, "straight")) {
        add_seg(ai, near, hi, near + outward * d_seg[i])
      } else if (identical(style_conn, "elbow")) {
        add_seg(ai, near, ai, near + outward * d2[i])
        add_seg(ai, near + outward * d2[i], hi, near + outward * d2[i])
        add_seg(hi, near + outward * d2[i], hi, near + outward * d_seg[i])
      } else {
        ## "diagonal": stub, diagonal, stub
        add_seg(ai, near, ai, near + outward * d1[i])
        add_seg(ai, near + outward * d1[i], hi, near + outward * d2[i])
        add_seg(hi, near + outward * d2[i], hi, near + outward * d_seg[i])
      }
    }

    if (length(a_start) > 0L) {
      start_xy <- to_xy(a_start, c_start)
      end_xy <- to_xy(a_end, c_end)
      conn_grob <- grid::segmentsGrob(
        x0 = start_xy$x, y0 = start_xy$y,
        x1 = end_xy$x, y1 = end_xy$y,
        gp = gp_conn
      )
    }
  }

  children <- if (is.null(conn_grob)) {
    do.call(grid::gList, text_grobs)
  } else {
    do.call(grid::gList, c(list(conn_grob), text_grobs))
  }
  grid::setChildren(x, children)
}
