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
#'   of the label height.  Defaults to `0.25`.
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
#' @return A `guide` object that can be used in `guides()` or in the `guide`
#'   argument of a scale.
#'
#' @seealso [smart_align()], [smart_stack()], [ggplot2::guide_axis()]
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
    available_aes    = c("x", "y"),
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
  panel_width      = NULL,
  max_nrow         = 8L,
  hjust            = NULL,
  vjust            = NULL
)

#' @rdname guide_axis_smart
#' @format NULL
#' @usage NULL
#' @export
GuideAxisSmart <- ggplot2::ggproto(
  "GuideAxisSmart",
  ggplot2::GuideAxis,
  params = c(ggplot2::GuideAxis$params, .ggse_guide_params),

  ## -- ticks ---------------------------------------------------------------
  build_ticks = function(self, key, elements, params,
                         position = params$opposite) {
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
    missing_labels <- setdiff(which, labels)
    if (length(missing_labels) > 0L) {
      cli::cli_warn(c(
        "Some labels requested in {.arg which} are not part of the axis.",
        "x" = "Not found: {.val {missing_labels}}."
      ))
    }
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

  if (isTRUE(getOption("ggsmartext.debug"))) {
    message(
      "ggsmartext: labels=", length(labels), " shown=", sum(shown),
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
