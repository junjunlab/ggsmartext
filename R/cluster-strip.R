## ---------------------------------------------------------------------------
## Facet strips: keep the cluster tree inside the strip band
## ---------------------------------------------------------------------------
## ggplot2 draws axis guides *outside* the strip band, so a tree attached with
## `position = "top"` ends up above the strip when the plot is facetted.  The
## helper below rewrites the finished gtable: the whole top axis (tree *and*
## labels) is moved into a fresh row directly below the strip, which gives the
## usual heatmap look -- strip, tree, labels, panel -- and keeps the labels
## hugging the panel instead of pushing them to the very top of the plot.

## Locate the tree inside an axis grob (an `absoluteGrob` wrapping a gtable).
.ggcl_tree_info <- function(grob) {
  if (!inherits(grob, "gTree") || !length(grob$children)) {
    return(NULL)
  }
  for (k in seq_along(grob$children)) {
    ch <- grob$children[[k]]
    if (!inherits(ch, "gtable")) {
      next
    }
    for (j in seq_along(ch$grobs)) {
      tj <- ch$grobs[[j]]
      if (!identical(tj$name, "ggutils.cluster.tree")) {
        next
      }
      along <- if (dim(ch)[2L] == 1L) "row" else "col"
      cell <- ch$layout[j, , drop = FALSE]
      return(list(
        child  = k,
        cell   = j,
        along  = along,
        index  = if (along == "row") cell$t else cell$l,
        grob   = tj,
        height = attr(tj, "ggse_size_cm") %||% 1
      ))
    }
  }
  NULL
}

## Find the strip that shares the panel column with an axis guide.
## Only the perpendicular extent is matched: for a top axis the strip can sit
## either just above or just below the axis row depending on the ggplot2 layout,
## so the closest strip row is picked instead of requiring an overlap.
.ggcl_strip_index <- function(lay, side, cell) {
  idx <- which(startsWith(lay$name, paste0("strip-", side, "-")))
  if (!length(idx)) {
    return(NA_integer_)
  }
  if (side %in% c("t", "b")) {
    idx <- idx[lay$l[idx] <= cell$r & lay$r[idx] >= cell$l]
  } else {
    idx <- idx[lay$t[idx] <= cell$b & lay$b[idx] >= cell$t]
  }
  if (!length(idx)) {
    return(NA_integer_)
  }
  idx[which.min(abs(lay$t[idx] - cell$t) + abs(lay$l[idx] - cell$l))]
}

## Rewrite a finished gtable so a top cluster tree sits between the strip and
## the axis labels.
##
## Inside the axis guide the tree is already stacked on the *outside* of the
## labels (title -> tree -> labels -> ticks -> panel), so the only thing left to
## fix for facets is the vertical order of the axis row and the strip row: the
## axis row is moved below the strip instead of staying above it.
.ggcl_trees_to_strip <- function(g) {
  lay <- g$layout
  axis_i <- which(grepl("^axis-t-", lay$name))
  if (!length(axis_i)) {
    return(g)
  }
  ## No facets: the tree already sits between the labels and the axis title.
  if (!any(grepl("^strip-t-", lay$name))) {
    return(g)
  }

  has_tree <- vapply(
    g$grobs[axis_i], function(z) !is.null(.ggcl_tree_info(z)), logical(1)
  )
  if (!any(has_tree)) {
    return(g)
  }
  ## Only panels that really carry a top strip need the rewrite.
  has_strip <- vapply(axis_i, function(i) {
    !is.na(.ggcl_strip_index(lay, "t", lay[i, , drop = FALSE]))
  }, logical(1))
  ## Move the full row (not just the panels that carry a tree) so every label
  ## keeps the same distance to the panel.
  move_i <- axis_i[has_strip]
  if (!length(move_i)) {
    return(g)
  }

  ## Multi-row facets (`facet_wrap(nrow = 3)`) have one top axis row per panel
  ## row, each belonging to its own strip.  Instead of inserting rows one by one
  ## (which shifts every later index), compute the final row order and permute
  ## the gtable in one go: each strip is immediately followed by the axis row
  ## that carries its tree.
  rows <- unique(lay$t[move_i])
  strip_of <- vapply(rows, function(r) {
    i <- move_i[lay$t[move_i] == r]
    s <- vapply(i, function(k) {
      j <- .ggcl_strip_index(lay, "t", lay[k, , drop = FALSE])
      ## `.ggcl_strip_index()` returns a row of `lay`; the permutation below
      ## works on row numbers, so translate it.
      if (is.na(j)) NA_integer_ else as.integer(lay$t[j])
    }, integer(1))
    s <- s[!is.na(s)]
    if (!length(s)) NA_integer_ else min(s)
  }, integer(1))
  keep <- !is.na(strip_of)
  rows <- rows[keep]
  strip_of <- strip_of[keep]
  if (!length(rows)) {
    return(g)
  }

  after <- split(rows, strip_of)
  axis_rows <- unlist(after, use.names = FALSE)
  n <- nrow(g)
  ord <- integer(0)
  for (r in seq_len(n)) {
    if (r %in% axis_rows) {
      ## Emitted right after its strip.
      next
    }
    ord <- c(ord, r)
    key <- as.character(r)
    if (key %in% names(after)) {
      ord <- c(ord, after[[key]])
    }
  }
  if (length(ord) != n) {
    return(g)
  }
  perm <- integer(n)
  perm[ord] <- seq_len(n)
  g$heights <- g$heights[ord]
  g$layout$t <- perm[g$layout$t]
  g$layout$b <- perm[g$layout$b]
  g
}

## ---------------------------------------------------------------------------
## Plot hooks
## ---------------------------------------------------------------------------
## The scale marks the plot so the gtable can be post-processed without asking
## the user to call anything extra.

#' Keep cluster trees inside the facet strip
#'
#' `scale_x_cluster()` and `scale_y_cluster()` mark the plot automatically, so
#' trees attached with `position = "top"` are moved between the strip and the
#' panel as soon as the plot is drawn.  When the tree comes from a second guide
#' added with [ggplot2::guides] (for example
#' `guides(x.top = guide_axis_cluster(...))`), wrap the plot in
#' `strip_cluster()` once to get the same layout.
#'
#' @param plot A `ggplot` object.
#' @return The same plot with the strip layout enabled.
#' @export
#' @examples
#' \dontrun{
#' p <- ggplot(df, aes(gene, sample)) +
#'   geom_tile() +
#'   facet_wrap(~ celltype, scales = "free") +
#'   guides(x.top = guide_axis_cluster(mat, free = TRUE, position = "top"))
#' strip_cluster(p)
#' }
strip_cluster <- function(plot) {
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a {.cls ggplot} object.")
  }
  cls <- class(plot)
  if (!"ggutils_cluster_plot" %in% cls) {
    class(plot) <- c("ggutils_cluster_plot", cls)
  }
  plot
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ggutils_cluster_scale <- function(object, plot, ...) {
  if (inherits(object, "ggutils_cluster_deferred")) {
    ## `data` was omitted: derive it from the plot's own data and mapping, then
    ## add the real scale.  The recursion re-enters this method with a plain
    ## scale, which falls through to `NextMethod()`.
    built <- .ggcl_materialise(object, plot)
    plot$data <- built$data
    return(plot + built$scale)
  }
  ## A tree that has to sit away from the labels is not drawn by the axis guide
  ## (a discrete secondary axis cannot carry a free panel's categories); the
  ## spec travels with the plot so `.ggcl_inject_trees()` can draw it in the
  ## gtable, where the panel geometry is known.
  spec <- attr(object, "ggcl_spec")
  plot <- NextMethod()
  if (!is.null(spec)) {
    ## A scale added with an explicit matrix never passes through
    ## `.ggcl_materialise()`, so honour the facet here too: `scales = "free_x"`
    ## is enough to ask for per-panel trees.
    if (is.null(spec$free)) {
      inferred <- .ggcl_facet_free(plot, spec$axis)
      if (!is.null(inferred)) spec$free <- inferred
    }
    plot$ggutils_cluster_specs <- c(plot$ggutils_cluster_specs, list(spec))
  }
  cls <- class(plot)
  if (!"ggutils_cluster_plot" %in% cls) {
    class(plot) <- c("ggutils_cluster_plot", cls)
  }
  plot
}

## Free facets re-key the focal column as `"<panel>__ggcl__<category>"` so each
## panel keeps its own leaf order.  The prefix stays in the scale (the trees
## match panels on it) and is removed from the rendered text here.
.ggcl_strip_axis_text <- function(x) {
  if (!is.null(x$label) && is.character(x$label) &&
      any(grepl(.ggcl_sep, x$label, fixed = TRUE))) {
    x$label <- .ggcl_strip_key(x$label)
  }
  if (inherits(x, "gtable")) {
    x$grobs <- lapply(x$grobs, .ggcl_strip_axis_text)
  } else if (inherits(x, "gTree") && length(x$children)) {
    x$children <- lapply(x$children, .ggcl_strip_axis_text)
  }
  x
}

## ---------------------------------------------------------------------------
## Tree injection
## ---------------------------------------------------------------------------
## `position = "top"` / `"right"` puts the tree on the other side of the panel
## while the category names stay on the usual axis.  That cannot be expressed
## with `sec.axis` (see the scales above), so the tree is drawn straight into
## the finished gtable: one extra row above each panel row, or one extra column
## right of each panel column.

.ggcl_inject_trees <- function(g, built) {
  specs <- built$plot$ggutils_cluster_specs
  if (!length(specs)) return(g)
  ## A spec the axis guide already drew (`draw_in_guide`) must not be drawn a
  ## second time above / beside the panel.
  specs <- Filter(function(s) !isTRUE(s$draw_in_guide), specs)
  if (!length(specs)) return(g)
  for (spec in specs) {
    if (identical(spec$position, "top")) {
      g <- .ggcl_inject_axis_trees(g, built, spec, side = "top")
    } else if (identical(spec$position, "left")) {
      g <- .ggcl_inject_axis_trees(g, built, spec, side = "left")
    } else if (identical(spec$position, "right")) {
      g <- .ggcl_inject_axis_trees(g, built, spec, side = "right")
    }
  }
  g
}

.ggcl_inject_axis_trees <- function(g, built, spec, side) {
  lay <- g$layout
  ## Panel cells are named `panel-<col>-<row>`, or just `panel` without facets.
  pl <- which(grepl("^panel($|-[0-9]+-[0-9]+$)", lay$name))
  if (!length(pl)) return(g)
  meta <- built$layout$layout
  pp <- built$layout$panel_params
  if (is.null(meta) || !length(pp)) return(g)

  parsed <- regmatches(
    lay$name[pl], regexec("^panel-([0-9]+)-([0-9]+)$", lay$name[pl])
  )
  col <- vapply(parsed, function(z) {
    if (length(z) >= 3L) as.integer(z[2]) else 1L
  }, integer(1))
  row <- vapply(parsed, function(z) {
    if (length(z) >= 3L) as.integer(z[3]) else 1L
  }, integer(1))

  cells <- data.frame(
    t = lay$t[pl], b = lay$b[pl], l = lay$l[pl], r = lay$r[pl],
    row = row, col = col, panel = NA_integer_, stringsAsFactors = FALSE
  )
  for (k in seq_len(nrow(cells))) {
    hit <- if (all(c("ROW", "COL") %in% names(meta))) {
      which(meta$ROW == cells$row[k] & meta$COL == cells$col[k])
    } else if (nrow(meta) == 1L) {
      1L
    } else {
      integer(0)
    }
    if (length(hit)) cells$panel[k] <- hit[1]
  }

  ## `facet_wrap(nrow = 2)` leaves an empty cell in the last row; it has a
  ## gtable slot but no panel, so it must not get a tree or an extra row.
  cells <- cells[!is.na(cells$panel), , drop = FALSE]
  if (!nrow(cells)) return(g)

  grobs <- lapply(seq_len(nrow(cells)), function(k) {
    .ggcl_tree_grob_for_panel(cells$panel[k], pp, spec, side)
  })

  if (identical(side, "top")) {
    ## `strip_side = "above"` puts the tree outside the strip; the default
    ## ("below") slides it between the strip and the panel.
    above <- identical(spec$strip_side %||% "below", "above")
    ## Bottom-up, so inserting a row never shifts the rows still to handle.
    for (r in sort(unique(cells$t), decreasing = TRUE)) {
      pos <- r - 1L
      if (above) {
        s <- vapply(which(cells$t == r), function(k) {
          j <- .ggcl_strip_index(lay, "t", cells[k, , drop = FALSE])
          if (is.na(j)) NA_integer_ else as.integer(lay$t[j])
        }, integer(1))
        s <- s[!is.na(s)]
        if (length(s)) pos <- min(s) - 1L
      }
      g <- gtable::gtable_add_rows(
        g, heights = grid::unit(spec$height, "cm"), pos = pos
      )
      ## `gtable_add_rows()` inserted the new row right *after* `pos`; the
      ## original row index `r` is only correct for the "below" case, where the
      ## insertion point is `r - 1`.
      at <- pos + 1L
      idx <- which(cells$t == r)
      for (k in idx) {
        g <- gtable::gtable_add_grob(
          g, grobs[[k]],
          t = at, b = at, l = cells$l[k], r = cells$r[k],
          name = paste0("ggutils.tree-", cells$row[k], "-", cells$col[k]),
          clip = "off"
        )
      }
    }
  } else if (identical(side, "left")) {
    ## One tree per *visible* y axis.  A panel that shares the row's y axis gets
    ## an axis slot with no labels in it, so its slot is only as wide as the
    ## tick; those panels must not receive a tree (`free_y` panels do, because
    ## they print their own labels).
    target_all <- vapply(seq_len(nrow(cells)), function(k) {
      ## Axis cells are named `axis-l-<row>-<col>` (row first), unlike panels.
      nm <- sprintf("axis-l-%d-%d", cells$row[k], cells$col[k])
      hit <- which(lay$name == nm)
      as.integer(if (length(hit)) lay$l[hit[1]] else cells$l[k] - 1L)
    }, integer(1))
    wide <- grid::convertWidth(g$widths[target_all], "cm", valueOnly = TRUE)
    sel <- which(!is.na(wide) & wide > 0.2)
    if (!length(sel)) return(g)
    target <- target_all[sel]
    for (cc in sort(unique(target), decreasing = TRUE)) {
      g <- gtable::gtable_add_cols(
        g, widths = grid::unit(spec$height, "cm"), pos = cc - 1L
      )
      for (m in which(target == cc)) {
        k <- sel[m]
        g <- gtable::gtable_add_grob(
          g, grobs[[k]],
          t = cells$t[k], b = cells$b[k], l = cc, r = cc,
          name = paste0("ggutils.tree-y-", cells$row[k], "-", cells$col[k]),
          clip = "off"
        )
      }
    }
  } else {
    ## `right`: one tree at the end of each panel row, right after that row's
    ## last panel (rows of a `facet_wrap` can end at different columns).
    picked <- vapply(sort(unique(cells$row)), function(r) {
      idx <- which(cells$row == r)
      idx[which.max(cells$col[idx])]
    }, integer(1))
    ord <- picked[order(cells$r[picked], decreasing = TRUE)]
    for (k in ord) {
      pos <- cells$r[k]
      g <- gtable::gtable_add_cols(
        g, widths = grid::unit(spec$height, "cm"), pos = pos
      )
      g <- gtable::gtable_add_grob(
        g, grobs[[k]],
        t = cells$t[k], b = cells$b[k], l = pos + 1L, r = pos + 1L,
        name = paste0("ggutils.tree-y-", cells$row[k], "-", cells$col[k]),
        clip = "off"
      )
    }
  }
  g
}

## Dendrogram for one panel, positioned on the panel's own break positions.
.ggcl_tree_grob_for_panel <- function(panel, pp, spec, side) {
  if (is.null(panel) || is.na(panel) || panel > length(pp)) {
    return(grid::nullGrob())
  }
  view <- if (identical(side, "top")) pp[[panel]]$x else pp[[panel]]$y
  if (is.null(view)) return(grid::nullGrob())
  brks <- as.character(view$get_breaks())
  if (length(brks) < 2L) return(grid::nullGrob())

  cr <- view$continuous_range
  if (is.null(cr) || length(cr) != 2L || !all(is.finite(cr)) ||
      diff(cr) == 0) {
    cr <- c(0.5, length(brks) + 0.5)
  }
  npc <- (seq_along(brks) - cr[1]) / diff(cr)

  ## The re-keyed factor keeps each panel's own tree, which is also what the
  ## axis order was built from.  With an explicitly supplied matrix there is no
  ## per-panel tree to reuse: take this panel's rows out of the matrix and
  ## cluster them, falling back to the order-preserving merge when the axis
  ## order cannot follow the standard leaf order.
  hc <- NULL
  if (isTRUE(spec$free)) {
    ## A re-keyed level is `<facet vars, sep-joined>__ggcl__<category>`.  Drop
    ## only the *last* part: `facet_grid()` joins two facet variables with the
    ## same separator, so truncating at the first one would lose a variable and
    ## miss the panel's own `hclust`.
    parts <- strsplit(brks[1], .ggcl_sep, fixed = TRUE)[[1]]
    key <- paste(parts[-length(parts)], collapse = .ggcl_sep)
    hc <- spec$panel_hclust[[key]]
    if (is.null(hc) && !is.null(spec$mat)) {
      idx <- match(.ggcl_strip_key(brks), rownames(spec$mat))
      if (!anyNA(idx)) {
        sub <- spec$mat[idx, , drop = FALSE]
        if (nrow(sub) >= 2L) {
          method <- spec$method %||% "complete"
          hc_std <- stats::hclust(stats::dist(sub), method = method)
          hc <- if (identical(rownames(sub)[hc_std$order], rownames(sub))) {
            hc_std
          } else {
            .ggcl_ordered_hclust(sub, method = method)
          }
          hc$labels <- rownames(sub)
        }
      }
    }
  }
  if (is.null(hc) || !inherits(hc, "hclust")) hc <- spec$hclust
  if (is.null(hc) || !inherits(hc, "hclust")) return(grid::nullGrob())

  lab <- .ggcl_strip_key(hc$labels %||% as.character(seq_along(hc$order)))
  leaf <- npc[match(lab, .ggcl_strip_key(brks))]
  if (anyNA(leaf)) {
    if (length(lab) != length(npc)) return(grid::nullGrob())
    leaf <- npc[seq_along(lab)]
  }
  .ggcl_tree_grob_at(leaf, hc, spec)
}

## Same dendrogram arithmetic as `.ggcl_tree_from_key()`, but fed with the
## leaf positions directly (npc along the axis, centimetres across it).
.ggcl_tree_grob_at <- function(leaf, hc, spec) {
  n <- length(leaf)
  if (n < 2L || length(hc$order) != n) return(grid::nullGrob())
  geom <- .ggcl_axis_geometry(spec$position)
  height <- spec$height %||% 1
  heights <- as.numeric(hc$height)
  hmax <- max(c(heights, 1e-8))
  h <- heights / hmax
  node <- numeric(n - 1L)
  for (i in seq_len(n - 1L)) {
    a <- hc$merge[i, 1L]
    b <- hc$merge[i, 2L]
    xa <- if (a < 0L) leaf[-a] else node[a]
    xb <- if (b < 0L) leaf[-b] else node[b]
    node[i] <- (xa + xb) / 2
  }

  ax <- ay <- bx <- by <- numeric(0)
  across_leaf <- geom$near
  for (i in seq_len(n - 1L)) {
    a <- hc$merge[i, 1L]
    b <- hc$merge[i, 2L]
    xa <- if (a < 0L) leaf[-a] else node[a]
    xb <- if (b < 0L) leaf[-b] else node[b]
    ha <- if (a < 0L) across_leaf else geom$near + geom$outward * h[a]
    hb <- if (b < 0L) across_leaf else geom$near + geom$outward * h[b]
    hp <- geom$near + geom$outward * h[i]
    if (identical(spec$type, "triangle")) {
      if (isTRUE(geom$vertical)) {
        ax <- c(ax, ha, hb); ay <- c(ay, xa, xb)
        bx <- c(bx, hp, hp); by <- c(by, node[i], node[i])
      } else {
        ax <- c(ax, xa, xb); ay <- c(ay, ha, hb)
        bx <- c(bx, node[i], node[i]); by <- c(by, hp, hp)
      }
    } else if (isTRUE(geom$vertical)) {
      ax <- c(ax, ha, hb, hp); ay <- c(ay, xa, xb, xa)
      bx <- c(bx, hp, hp, hp); by <- c(by, xa, xb, xb)
    } else {
      ax <- c(ax, xa, xb, xa); ay <- c(ay, ha, hb, hp)
      bx <- c(bx, xa, xb, xb); by <- c(by, hp, hp, hp)
    }
  }

  gap <- .ggcl_tree_gap(spec)
  segments <- grid::segmentsGrob(
    x0 = if (isTRUE(geom$vertical)) .ggcl_depth_unit(ax, height, geom, gap) else grid::unit(ax, "npc"),
    y0 = if (isTRUE(geom$vertical)) grid::unit(ay, "npc") else .ggcl_depth_unit(ay, height, geom, gap),
    x1 = if (isTRUE(geom$vertical)) .ggcl_depth_unit(bx, height, geom, gap) else grid::unit(bx, "npc"),
    y1 = if (isTRUE(geom$vertical)) grid::unit(by, "npc") else .ggcl_depth_unit(by, height, geom, gap),
    gp = grid::gpar(
      col = spec$col %||% "black",
      lwd = spec$lwd %||% 0.7,
      lineend = "round"
    )
  )
  .ggcl_tree_grob(segments, height)
}

## Remove the axis title on the side that carries the tree, so the labels side
## keeps the only title.  Detection is done on the finished gtable (the tree is
## visibly there) instead of on scale attributes: ggplot2 clones scales while
## building facets, which drops custom attributes.
.ggcl_blank_tree_title <- function(g) {
  sides <- c("axis-t" = "xlab-t", "axis-r" = "ylab-r")
  for (side in names(sides)) {
    idx <- which(startsWith(g$layout$name, paste0(side, "-")))
    if (!length(idx)) next
    has_tree <- any(vapply(
      g$grobs[idx], function(z) !is.null(.ggcl_tree_info(z)), logical(1)
    ))
    if (!isTRUE(has_tree)) next
    i <- match(unname(sides[[side]]), g$layout$name)
    if (is.na(i)) next
    g$grobs[[i]] <- grid::nullGrob()
    if (startsWith(unname(sides[[side]]), "xlab")) {
      g$heights[g$layout$t[i]] <- grid::unit(0, "cm")
    } else {
      g$widths[g$layout$l[i]] <- grid::unit(0, "cm")
    }
  }
  g
}

#' @exportS3Method ggplot2::ggplot_build
ggplot_build.ggutils_cluster_plot <- function(plot) {
  out <- NextMethod()
  class(out) <- c("ggutils_cluster_built", class(out))
  out
}

#' @exportS3Method ggplot2::ggplot_gtable
ggplot_gtable.ggutils_cluster_built <- function(data) {
  g <- NextMethod()
  g <- .ggcl_strip_axis_text(g)
  if (inherits(data$plot$coordinates, "CoordRadial")) {
    ## A theta axis runs around a circle: the tree is a ring dendrogram drawn
    ## into each panel, not a strip above / beside it.
    g <- .ggcl_ring_trees_to_gtable(g, data)
  } else {
    g <- .ggcl_inject_trees(g, data)
    g <- .ggcl_blank_tree_title(g)
    g <- .ggcl_trees_to_strip(g)
  }
  g
}
