## ---------------------------------------------------------------------------
## Clustering data derived from the plot itself
## ---------------------------------------------------------------------------
## The public API does not ask for a clustering matrix: `scale_x_cluster()` /
## `scale_y_cluster()` without `data` read the plot's own data and mapping, and
## build the matrix by reshaping the focal axis against the other axis using a
## numeric aesthetic (`fill`, `colour`, `size`, ...) as the value.  When no such
## aesthetic is mapped, the other axis is summarised per category instead.

## Resolve an aesthetic mapping to a column name of `data`.  Only plain column
## references are understood: `aes(x = gene)` or `aes(x = .data$gene)`.
.ggcl_aes_var <- function(mapping, aes, data) {
  q <- mapping[[aes]]
  if (is.null(q)) return(NULL)
  expr <- rlang::quo_get_expr(q)
  if (rlang::is_null(expr)) return(NULL)
  nm <- if (rlang::is_symbol(expr)) {
    rlang::as_name(expr)
  } else {
    lab <- rlang::as_label(expr)
    ## `aes(x = .data$gene)` labels as ".data$gene"; keep the bare name.
    sub("^\\.data\\$", "", lab)
  }
  if (!nm %in% names(data)) return(NULL)
  nm
}

## Pick the numeric column that carries the values to cluster on.
.ggcl_pick_value <- function(data, mapping, value = NULL) {
  if (!is.null(value)) {
    if (!is.character(value) || length(value) != 1L) {
      cli::cli_abort("{.arg value} must be a single column name.")
    }
    if (!value %in% names(data)) {
      cli::cli_abort("{.arg value} ({.val {value}}) is not a column of the plot data.")
    }
    if (!is.numeric(data[[value]])) {
      cli::cli_abort("{.arg value} ({.val {value}}) must be a numeric column.")
    }
    return(value)
  }
  candidates <- c("fill", "colour", "color", "size", "linewidth", "alpha", "weight")
  for (a in candidates) {
    v <- .ggcl_aes_var(mapping, a, data)
    if (!is.null(v) && is.numeric(data[[v]])) return(v)
  }
  NULL
}

## Aesthetics are often mapped on the first layer rather than in `ggplot()`;
## merge the plot mapping with the layer mappings so the auto-derived matrix
## works for both styles.
.ggcl_plot_mapping <- function(plot) {
  mapping <- plot$mapping %||% ggplot2::aes()
  aes_all <- c("x", "y", "fill", "colour", "color", "size", "linewidth",
               "alpha", "weight")
  for (ly in plot$layers %||% list()) {
    ly_map <- ly$mapping %||% ggplot2::aes()
    for (a in aes_all) {
      if (is.null(mapping[[a]]) && !is.null(ly_map[[a]])) {
        mapping[[a]] <- ly_map[[a]]
      }
    }
  }
  mapping
}

## Reshape a long frame (`.focal`, `.other`, `.value`) into a wide matrix.
## Empty focal/other combinations are filled with the row mean so that
## `stats::dist()` does not silently drop categories (which would take them off
## the axis).
.ggcl_matrix_from_long <- function(long) {
  m <- tapply(long$.value, list(long$.focal, long$.other), mean, na.rm = TRUE)
  m <- matrix(as.numeric(m), nrow = nrow(m), dimnames = dimnames(m))
  if (anyNA(m)) {
    rm <- rowMeans(m, na.rm = TRUE)
    rm[!is.finite(rm)] <- 0
    for (j in seq_len(ncol(m))) {
      na <- is.na(m[, j])
      if (any(na)) m[na, j] <- rm[na]
    }
  }
  m
}

## Build the clustering matrix from the plot data.  Returns the matrix plus the
## long frame (when the matrix came from a reshape), which the guide reuses to
## re-cluster each free-scale panel without gaps.
.ggcl_data_from_plot <- function(data, mapping, focal = "x", value = NULL) {
  focal <- match.arg(focal, c("x", "y"))
  other <- if (focal == "x") "y" else "x"
  fv <- .ggcl_aes_var(mapping, focal, data)
  ov <- .ggcl_aes_var(mapping, other, data)
  if (is.null(fv) || is.null(ov)) {
    cli::cli_abort(c(
      "Cannot derive the clustering matrix from the plot.",
      i = "Map both axes to columns of the data, e.g. {.code aes(x = gene, y = sample)}.",
      i = "Otherwise pass the matrix explicitly: {.code data = mat}."
    ))
  }
  f <- data[[fv]]
  o <- data[[ov]]
  vv <- .ggcl_pick_value(data, mapping, value)
  if (!is.null(vv)) {
    keep <- !is.na(f) & !is.na(o) & !is.na(data[[vv]])
    if (!any(keep)) {
      cli::cli_abort("No complete rows to cluster on.")
    }
    long <- data.frame(
      .focal = as.character(f[keep]),
      .other = as.character(o[keep]),
      .value = as.numeric(data[[vv]][keep]),
      stringsAsFactors = FALSE
    )
    return(list(
      mat = .ggcl_matrix_from_long(long), long = long,
      vars = c(focal = fv, other = ov, value = vv)
    ))
  }
  ## No numeric aesthetic: summarise the other axis within each category.
  if (is.numeric(o)) {
    sp <- split(o, as.character(f))
    mat <- t(vapply(sp, function(z) {
      c(mean = mean(z, na.rm = TRUE), sd = stats::sd(z, na.rm = TRUE),
        median = stats::median(z, na.rm = TRUE), n = length(z))
    }, numeric(4)))
    mat[!is.finite(mat)] <- 0
    return(list(mat = mat, long = NULL,
                vars = c(focal = fv, other = ov, value = NA_character_)))
  }
  ## Both axes categorical and nothing numeric mapped: cluster on co-occurrence.
  tb <- table(as.character(f), as.character(o))
  mat <- matrix(as.numeric(tb), nrow = nrow(tb), dimnames = dimnames(tb))
  cli::cli_inform(c(
    "No numeric aesthetic found; clustering {.val {fv}} on how often it co-occurs with {.val {ov}}.",
    i = "Map a numeric variable to {.code fill}/{.code colour}/{.code size} for a meaningful tree."
  ))
  list(mat = mat, long = NULL, vars = c(focal = fv, other = ov, value = NA_character_))
}

## ---------------------------------------------------------------------------
## Re-keying the focal column for free facets
## ---------------------------------------------------------------------------
## A free facet gives every panel its own leaf order, but a discrete scale's
## limits are global: there is no way to hand panel A and panel B different
## category orders.  The categories themselves are therefore made per-panel
## values -- `"<panel>__ggcl__<category>"` -- with the factor levels ordered by
## each panel's own dendrogram.  `free_x`/`free_y` then pick each panel's own
## levels in that order, so the axis really is in leaf order and a standard
## `hclust()` can be drawn without a single crossing branch.  The prefix is
## stripped from the rendered axis text at gtable time.

.ggcl_sep <- "__ggcl__"

## Facet variables of the plot, as column names of `plot$data`.
.ggcl_facet_vars <- function(plot) {
  par <- plot$facet$params %||% list()
  qs <- c(par$facets %||% list(), par$rows %||% list(), par$cols %||% list())
  if (!length(qs)) return(character(0))
  vars <- vapply(qs, function(q) {
    if (rlang::is_quosure(q)) {
      rlang::as_label(rlang::quo_get_expr(q))
    } else {
      rlang::as_label(q)
    }
  }, character(1))
  intersect(vars, names(plot$data))
}

## `"A__ggcl__g1"` -> `"g1"`; values without a prefix are returned unchanged.
.ggcl_strip_key <- function(x) {
  vapply(
    strsplit(as.character(x), .ggcl_sep, fixed = TRUE),
    function(part) part[[length(part)]],
    character(1)
  )
}

## Whether the facet lets this axis vary per panel, e.g. `scales = "free_x"`.
## `NULL` when the plot has no faceting at all.
.ggcl_facet_free <- function(plot, axis) {
  fr <- plot$facet$params$free
  if (is.null(fr)) return(NULL)
  if (is.list(fr)) isTRUE(fr[[axis]]) else isTRUE(fr)
}

## Re-key the focal column panel by panel.  Returns the re-keyed data plus the
## per-panel `hclust` objects, which the tree drawing reuses so that the leaves
## keep exactly the order the factor levels promise.
.ggcl_rekey_panels <- function(plot, data, mapping, focal, method, value) {
  vars <- .ggcl_facet_vars(plot)
  if (!length(vars) || nrow(data) < 2L) return(NULL)
  fv <- .ggcl_aes_var(mapping, focal, data)
  if (is.null(fv)) return(NULL)

  panel_key <- do.call(
    paste,
    c(lapply(vars, function(v) as.character(data[[v]])), list(sep = .ggcl_sep))
  )
  groups <- split(seq_len(nrow(data)), panel_key)
  if (length(groups) < 2L) return(NULL)

  levels_keys <- character(0)
  panel_hclust <- list()
  for (g in names(groups)) {
    sub <- data[groups[[g]], , drop = FALSE]
    mat <- tryCatch(
      .ggcl_data_from_plot(sub, mapping, focal, value)$mat,
      error = function(e) NULL
    )
    if (is.null(mat) || nrow(mat) < 2L) {
      ord <- unique(as.character(sub[[fv]]))
      hc <- NULL
    } else {
      hc <- stats::hclust(
        stats::dist(mat), method = method %||% "complete"
      )
      ord <- rownames(mat)[hc$order]
    }
    panel_hclust[[g]] <- hc
    levels_keys <- c(levels_keys, paste0(g, .ggcl_sep, ord))
  }

  new_vals <- paste0(panel_key, .ggcl_sep, as.character(data[[fv]]))
  data[[fv]] <- factor(new_vals, levels = unique(levels_keys))
  list(
    data = data, levels = unique(levels_keys),
    panel_hclust = panel_hclust, var = fv
  )
}

## ---------------------------------------------------------------------------
## Contiguity-constrained clustering
## ---------------------------------------------------------------------------
## In a free facet the category order belongs to the data, so the axis cannot be
## re-sorted per panel.  A dendrogram whose leaves are not laid out in
## `hclust$order` draws crossing branches, so the per-panel tree is built by
## merging only *adjacent* clusters: the leaf order is then the axis order, the
## layout is planar by construction, and `hc$order` is the identity.

.ggcl_adjacent_dist <- function(mat, d, a, b, method) {
  if (method == "single") {
    min(d[a, b, drop = TRUE])
  } else if (method == "complete") {
    max(d[a, b, drop = TRUE])
  } else if (method %in% c("ward.D", "ward.D2")) {
    ca <- colMeans(mat[a, , drop = FALSE])
    cb <- colMeans(mat[b, , drop = FALSE])
    ## Ward: n_i n_j / (n_i + n_j) * squared centroid distance.
    length(a) * length(b) / (length(a) + length(b)) * sum((ca - cb)^2)
  } else {
    mean(d[a, b, drop = TRUE])
  }
}

.ggcl_ordered_hclust <- function(mat, method = "complete") {
  n <- nrow(mat)
  labs <- rownames(mat)
  if (is.null(labs)) labs <- as.character(seq_len(n))
  d <- as.matrix(stats::dist(mat))
  clusters <- lapply(seq_len(n), function(i) i)   # leaves, in axis order
  ids <- seq_len(n)                                # node id of each cluster
  heights <- numeric(0)
  merge <- matrix(0L, n - 1L, 2L)
  next_id <- n + 1L

  while (length(clusters) > 1L) {
    k <- length(clusters)
    dists <- vapply(seq_len(k - 1L), function(i) {
      .ggcl_adjacent_dist(mat, d, clusters[[i]], clusters[[i + 1L]], method)
    }, numeric(1))
    i <- which.min(dists)
    a <- clusters[[i]]
    b <- clusters[[i + 1L]]
    node <- next_id
    next_id <- next_id + 1L
    ## `hclust$merge` refers to earlier merges by their ROW number, not by an
    ## arbitrary node id.
    row <- node - n
    merge[row, ] <- c(
      if (length(a) == 1L) -a else ids[[i]],
      if (length(b) == 1L) -b else ids[[i + 1L]]
    )
    heights[row] <- dists[[i]]
    clusters[[i]] <- c(a, b)
    ids[[i]] <- row
    clusters[[i + 1L]] <- NULL
    ids <- ids[-(i + 1L)]
  }
  ## hclust relies on non-decreasing heights.
  if (length(heights) > 1L) heights <- cummax(heights)
  structure(
    list(merge = merge, height = heights, order = seq_len(n),
         labels = labs, method = method, dist.method = "euclidean"),
    class = "hclust"
  )
}

## ---------------------------------------------------------------------------
## Deferred scale: build it once the plot (and thus the data) is known
## ---------------------------------------------------------------------------
## `+ scale_x_cluster()` happens on a plot object that already carries the data,
## but a scale is normally constructed before it is added.  When `data` is
## omitted the scale is a light placeholder that `ggplot_add()` materialises.

.ggcl_defer <- function(axis, params) {
  structure(
    list(axis = axis, params = params),
    class = c("ggutils_cluster_deferred", "ggutils_cluster_scale")
  )
}

.ggcl_materialise <- function(object, plot) {
  p <- object$params
  data <- plot$data
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "The plot has no data frame to cluster on.",
      i = "Pass the matrix explicitly: {.code scale_{object$axis}_cluster(data = mat)}."
    ))
  }
  mapping <- .ggcl_plot_mapping(plot)
  ## `free` is optional: with `facet_wrap(scales = "free_x")` the panels already
  ## disagree, so the per-panel behaviour is what the plot asks for.  An explicit
  ## `free = FALSE` still wins.
  if (is.null(p$free)) {
    inferred <- .ggcl_facet_free(plot, object$axis)
    if (!is.null(inferred)) p$free <- inferred
  }
  rekey <- NULL
  if (isTRUE(p$free)) {
    rekey <- .ggcl_rekey_panels(
      plot, data, mapping, object$axis, p$method, p$value
    )
    if (!is.null(rekey)) {
      data <- rekey$data
      ## A global `limits` cannot survive re-keying (it names the old values).
      p$limits <- NULL
      ## Named `labels` vectors have to be re-pointed at the re-keyed breaks.
      ## The prefix itself is stripped by the guide once the tree has been
      ## matched, so the measured text width stays honest.
      if (is.character(p$labels) && !is.null(names(p$labels))) {
        orig <- .ggcl_strip_key(rekey$levels)
        hit <- match(orig, names(p$labels))
        vals <- unname(p$labels)[hit]
        vals[is.na(hit)] <- orig[is.na(hit)]
        p$labels <- stats::setNames(vals, rekey$levels)
      }
      ## The y axis uses a plain `guide_axis()` while the tree is injected into
      ## the gtable.  Its re-keyed breaks ("panel__ggcl__0") would otherwise be
      ## measured at their full length, leaving a prefix-wide gap between the
      ## injected tree and the visible label.  Strip the prefix in the scale so
      ## the guide lays out the text the user actually sees.
      if (identical(object$axis, "y") &&
          (inherits(p$labels, "waiver") || is.null(p$labels))) {
        p$labels <- .ggcl_strip_key
      }
    }
  }
  derived <- .ggcl_data_from_plot(
    data = data, mapping = mapping,
    focal = object$axis, value = p$value
  )
  args <- c(
    list(data = derived$mat, .map_data = derived$long),
    p[setdiff(names(p), c("value", "dots"))]
  )
  if (length(p$dots)) args <- c(args, p$dots)
  scale <- if (identical(object$axis, "x")) {
    do.call(scale_x_cluster, args)
  } else {
    do.call(scale_y_cluster, args)
  }
  ## Everything the gtable stage needs to draw the tree where `position` asks
  ## for it.  The tree no longer travels through the axis guide when it has to
  ## sit away from the labels, so the spec carries the clustering itself.
  hc <- tryCatch(
    stats::hclust(stats::dist(derived$mat), method = p$method %||% "complete"),
    error = function(e) NULL
  )
  attr(scale, "ggcl_spec") <- list(
    axis = object$axis,
    position = p$tree_position %||% p$position,
    height = p$height %||% 1,
    col = p$col %||% "black",
    lwd = p$lwd %||% 0.7,
    type = as.character(p$type %||% "rectangle")[[1]],
    free = isTRUE(p$free),
    method = p$method %||% "complete",
    strip_side = as.character(p$strip_side %||% "below")[[1]],
    ring_gap = p$ring_gap,
    draw_in_guide = identical(object$axis, "x") &&
      !identical(p$tree_position %||% p$position, "top"),
    hclust = hc,
    panel_hclust = if (is.null(rekey)) NULL else rekey$panel_hclust
  )
  ## `data` may have been re-keyed for free facets; the plot has to use it too.
  list(scale = scale, data = data, rekeyed = !is.null(rekey))
}
