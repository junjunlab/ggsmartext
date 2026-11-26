## Clustering matrix derived from the plot data (no `data` argument).

make_df <- function(n_gene = 6, n_sample = 8, seed = 21) {
  set.seed(seed)
  df <- expand.grid(
    gene = paste0("g", seq_len(n_gene)),
    sample = paste0("s", seq_len(n_sample)),
    stringsAsFactors = FALSE
  )
  df$value <- rnorm(nrow(df))
  df$sample <- factor(df$sample, levels = paste0("s", seq_len(n_sample)))
  df
}

test_that("the matrix is derived from the mapped fill aesthetic", {
  df <- make_df()
  p <- ggplot2::ggplot(df, ggplot2::aes(gene, sample, fill = value)) +
    ggplot2::geom_tile()
  derived <- ggutils:::.ggcl_data_from_plot(
    df, ggutils:::.ggcl_plot_mapping(p), "x", NULL
  )
  manual <- tapply(df$value, list(df$gene, df$sample), mean)
  expect_equal(unname(derived$mat), unname(manual))
  expect_identical(rownames(derived$mat), paste0("g", 1:6))
  expect_identical(derived$vars[["value"]], "value")
})

test_that("a scale without data re-orders the axis like an explicit matrix", {
  df <- make_df()
  p_auto <- ggplot2::ggplot(df, ggplot2::aes(gene, sample)) +
    ggplot2::geom_point(ggplot2::aes(size = value)) +
    scale_x_cluster(method = "average")
  b <- ggplot2::ggplot_build(p_auto)
  labels <- b$layout$panel_params[[1]]$x$get_labels()
  manual <- tapply(df$value, list(df$gene, df$sample), mean)
  expect_identical(labels, cluster_order(manual, "average"))

  sx <- p_auto$scales$get_scales("x")
  expect_identical(sx$limits, cluster_order(manual, "average"))
})

test_that("`value` overrides the automatic aesthetic choice", {
  df <- make_df()
  df$other <- rev(df$value)
  derived <- ggutils:::.ggcl_data_from_plot(
    df, ggplot2::aes(gene, sample, fill = value, size = other), "x", "other"
  )
  expect_identical(derived$vars[["value"]], "other")
  expect_equal(
    unname(derived$mat),
    unname(tapply(df$other, list(df$gene, df$sample), mean))
  )
})

test_that("no numeric aesthetic falls back to summary statistics", {
  df <- make_df(n_gene = 4, n_sample = 5)
  derived <- ggutils:::.ggcl_data_from_plot(
    df, ggplot2::aes(gene, value), "x", NULL
  )
  expect_identical(colnames(derived$mat), c("mean", "sd", "median", "n"))
  expect_identical(rownames(derived$mat), paste0("g", 1:4))
  expect_true(is.numeric(derived$mat[, "mean"]))
})

test_that("gaps in the grid do not drop a category", {
  df <- make_df(n_gene = 4, n_sample = 4)
  df <- df[!(df$gene == "g2" & df$sample == "s3"), ]
  m <- ggutils:::.ggcl_matrix_from_long(
    data.frame(.focal = df$gene, .other = as.character(df$sample),
               .value = df$value)
  )
  expect_false(anyNA(m))
  expect_identical(nrow(m), 4L)
})

test_that("an explicit matrix still wins over the plot data", {
  df <- make_df()
  mat <- matrix(seq_len(12), nrow = 6,
                dimnames = list(paste0("g", 1:6), paste0("v", 1:2)))
  p <- ggplot2::ggplot(df, ggplot2::aes(gene, sample)) +
    ggplot2::geom_point(ggplot2::aes(size = value)) +
    scale_x_cluster(mat)
  sx <- p$scales$get_scales("x")
  expect_identical(sx$limits, cluster_order(mat, "complete"))
})

test_that("auto-derived free facets keep the panel's own categories", {
  set.seed(22)
  df <- rbind(
    data.frame(panel = "A", gene = rep(paste0("g", 1:4), 8),
               sample = factor(rep(paste0("s", 1:8), each = 4)), value = rnorm(32)),
    data.frame(panel = "B", gene = rep(paste0("g", 3:6), 7),
               sample = factor(rep(paste0("s", 1:7), each = 4)), value = rnorm(28))
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(gene, sample)) +
    ggplot2::geom_point(ggplot2::aes(size = value)) +
    ggplot2::facet_wrap(~panel, scales = "free_x") +
    scale_x_cluster(free = TRUE, position = "top")
  g <- ggplot2::ggplotGrob(p)
  expect_s3_class(g, "gtable")
  expect_true(any(grepl("^axis-t", g$layout$name)))
})

test_that("free facets use order-preserving clustering (no crossings)", {
  set.seed(31)
  m <- matrix(rnorm(24), nrow = 6,
              dimnames = list(paste0("g", 1:6), paste0("v", 1:4)))
  hc <- ggutils:::.ggcl_ordered_hclust(m, "complete")
  expect_s3_class(hc, "hclust")
  ## Leaves keep the axis order, which is what makes the layout planar.
  expect_identical(hc$order, 1:6)
  expect_identical(hc$labels, rownames(m))
  ## hclust contract: merge row numbers and non-decreasing heights.
  expect_identical(dim(hc$merge), c(5L, 2L))
  expect_true(all(hc$merge > -7L & hc$merge < 6L))
  expect_true(all(diff(hc$height) >= -1e-8))
  expect_s3_class(stats::as.dendrogram(hc), "dendrogram")
})

test_that("a free panel reorders its own categories into leaf order", {
  set.seed(32)
  df <- rbind(
    data.frame(panel = "A", gene = rep(paste0("g", 1:4), 8),
               sample = factor(rep(paste0("s", 1:8), each = 4)), value = rnorm(32)),
    data.frame(panel = "B", gene = rep(c("g2", "g3", "g5", "g6"), 7),
               sample = factor(rep(paste0("s", 1:7), each = 4)), value = rnorm(28))
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(gene, sample)) +
    ggplot2::geom_point(ggplot2::aes(size = value)) +
    ggplot2::facet_wrap(~panel, scales = "free_x") +
    scale_x_cluster(free = TRUE, position = "top")
  b <- ggplot2::ggplot_build(p)
  brks <- lapply(b$layout$panel_params, function(pp) pp$x$get_breaks())
  ## Every panel is re-keyed, so no two panels share a break value.
  expect_true(all(vapply(
    brks, function(z) all(grepl("__ggcl__", z, fixed = TRUE)), logical(1)
  )))
  ## The axis order of a panel is that panel's own dendrogram leaf order.
  leaf_order_of <- function(sub) {
    mat <- ggutils:::.ggcl_data_from_plot(
      sub, ggutils:::.ggcl_plot_mapping(p), "x", NULL
    )$mat
    hc <- stats::hclust(stats::dist(mat), "complete")
    rownames(mat)[hc$order]
  }
  expect_identical(
    ggutils:::.ggcl_strip_key(brks[[1]]),
    leaf_order_of(df[df$panel == "A", ])
  )
  expect_identical(
    ggutils:::.ggcl_strip_key(brks[[2]]),
    leaf_order_of(df[df$panel == "B", ])
  )
  ## ...and the panels disagree, which is the whole point of per-panel trees.
  expect_false(identical(
    ggutils:::.ggcl_strip_key(brks[[1]]),
    ggutils:::.ggcl_strip_key(brks[[2]])
  ))

  g <- ggplot2::ggplotGrob(p)
  ## The re-keyed prefix never reaches the rendered text.
  collect <- function(x) {
    out <- character(0)
    if (!is.null(x$label) && is.character(x$label)) out <- c(out, x$label)
    if (inherits(x, "gtable")) {
      out <- c(out, unlist(lapply(x$grobs, collect)))
    } else if (inherits(x, "gTree") && length(x$children)) {
      out <- c(out, unlist(lapply(x$children, collect)))
    }
    out
  }
  expect_false(any(grepl("__ggcl__", unlist(lapply(g$grobs, collect)),
                         fixed = TRUE)))

  count_tree <- function(x) {
    n <- as.integer(inherits(x, "ggutils_cluster_tree"))
    ch <- if (inherits(x, "gTree")) x$children else NULL
    if (length(ch)) n <- n + sum(vapply(ch, count_tree, integer(1)))
    if (inherits(x, "gtable")) n <- n + sum(vapply(x$grobs, count_tree, integer(1)))
    n
  }
  expect_identical(count_tree(g), 2L)
})
