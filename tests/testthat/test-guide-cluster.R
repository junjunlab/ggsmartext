test_that("cluster_order follows hclust leaf order", {
  set.seed(11)
  mat <- matrix(rnorm(20), nrow = 5,
                dimnames = list(letters[1:5], paste0("v", 1:4)))
  hc <- stats::hclust(stats::dist(mat), method = "complete")
  expect_equal(cluster_order(mat, "complete"), rownames(mat)[hc$order])
})

test_that("guide_axis_cluster stores the requested parameters", {
  set.seed(12)
  mat <- matrix(rnorm(24), nrow = 6,
                dimnames = list(paste0("g", 1:6), paste0("v", 1:4)))
  g <- guide_axis_cluster(mat, method = "average", height = 2.5,
                          col = "red", lwd = 1.2, type = "triangle")
  expect_identical(g$params$height, 2.5)
  expect_identical(g$params$tree_col, "red")
  expect_identical(g$params$lwd, 1.2)
  expect_identical(g$params$type, "triangle")
  expect_s3_class(g, "GuideAxisCluster")
})

test_that("cluster scales re-order categories and attach the guide", {
  set.seed(13)
  mat <- matrix(rnorm(30), nrow = 10,
                dimnames = list(paste0("g", 1:10), paste0("v", 1:3)))
  ord <- cluster_order(mat, "average")
  sx <- scale_x_cluster(mat, method = "average", type = "triangle")
  sy <- scale_y_cluster(mat, method = "average")
  expect_identical(sx$limits, ord)
  expect_identical(sy$limits, ord)
  expect_s3_class(sx$guide, "GuideAxisCluster")
  expect_identical(sx$guide$params$type, "triangle")
  ## On a y axis the tree is drawn in the gtable instead of the axis slot (the
  ## axis keeps the plain labels), so the scale carries an `ggcl_spec` and a
  ## stock axis guide.
  expect_s3_class(sy$guide, "GuideAxis")
  spec <- attr(sy, "ggcl_spec")
  expect_identical(spec$axis, "y")
  expect_identical(spec$type, "rectangle")
})

test_that("cluster guide renders on x and y for both tree shapes", {
  set.seed(14)
  mat <- matrix(rnorm(24), nrow = 8,
                dimnames = list(paste0("g", 1:8), paste0("s", 1:3)))
  long <- as.data.frame(as.table(mat))
  names(long) <- c("gene", "sample", "value")

  for (shape in c("rectangle", "triangle")) {
    px <- ggplot(long, aes(gene, sample, fill = value)) +
      geom_tile() +
      scale_x_cluster(mat, method = "average", height = 1,
                      col = "#b2182b", type = shape) +
      theme_minimal()
    py <- ggplot(long, aes(sample, gene, fill = value)) +
      geom_tile() +
      scale_y_cluster(mat, method = "ward.D2", height = 1,
                      col = "#2166ac", type = shape) +
      theme_minimal()
    expect_silent(ggplot2::ggplotGrob(px))
    expect_silent(ggplot2::ggplotGrob(py))
  }
})

test_that("cluster inputs are validated", {
  expect_error(guide_axis_cluster(), "Supply either")
  expect_error(guide_axis_cluster(matrix(1:4, nrow = 2), height = -1), "height")
  expect_error(cluster_order(data.frame(x = letters[1:3])), "numeric")
})

test_that("free = TRUE needs data and stores it", {
  expect_error(
    guide_axis_cluster(hclust = stats::hclust(stats::dist(matrix(rnorm(20), 5))),
                       free = TRUE),
    "needs"
  )
  set.seed(21)
  mat <- matrix(rnorm(6), nrow = 6, dimnames = list(paste0("g", 1:6), "v"))
  g <- guide_axis_cluster(mat, free = TRUE, method = "average")
  expect_true(g$params$free)
  expect_true(is.matrix(g$params$data_mat))
  expect_identical(g$params$method, "average")
})

test_that("cluster guide renders on native facetted free scales (x and y)", {
  set.seed(22)
  mat <- matrix(rnorm(12 * 6), nrow = 12,
                dimnames = list(paste0("g", 1:12), paste0("v", 1:6)))
  cl <- rep(c("A", "B"), each = 6)
  df <- data.frame(
    cluster = rep(cl, each = 6),
    gene    = unlist(lapply(cl, function(i) sample(rownames(mat), 6))),
    value   = rnorm(72),
    stringsAsFactors = FALSE
  )
  df$gene <- factor(df$gene, levels = rownames(mat))

  p <- ggplot(df, aes(gene, value)) +
    geom_point() +
    facet_wrap(~ cluster, scales = "free_x") +
    guides(x = guide_axis_cluster(data = mat, free = TRUE, height = 1,
                                   type = "triangle")) +
    theme_gray()
  expect_silent(ggplot2::ggplotGrob(p))

  set.seed(22)
  mat_y <- matrix(rnorm(12 * 6), nrow = 12,
                  dimnames = list(paste0("y", 1:12), paste0("v", 1:6)))
  df2 <- data.frame(
    cluster = rep(c("A", "B"), each = 36),
    ycat    = factor(sample(paste0("y", 1:12), 72, replace = TRUE)),
    value   = rnorm(72)
  )
  p2 <- ggplot(df2, aes(value, ycat)) +
    geom_point() +
    facet_wrap(~ cluster, scales = "free") +
    guides(y = guide_axis_cluster(data = mat_y, free = TRUE, height = 1)) +
    theme_gray()
  expect_silent(ggplot2::ggplotGrob(p2))
})


test_that("scale_x_cluster draws a ring tree on a coord_radial theta axis", {
  set.seed(21)
  mat <- matrix(rnorm(12 * 4), nrow = 12,
                dimnames = list(paste0("g", 1:12), paste0("s", 1:4)))
  long <- as.data.frame(as.table(mat))
  names(long) <- c("gene", "sample", "value")

  ## `position = "top"` puts the ring tree outside the circle.
  p <- ggplot2::ggplot(long, ggplot2::aes(gene, sample, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_radial(theta = "x") +
    scale_x_cluster(mat, method = "average", height = 1, position = "top")

  ## The theta axis must keep its labels and must not warn about the guide.
  expect_silent(ggplot2::ggplot_build(p))
  g <- ggplot2::ggplotGrob(p)
  expect_true(any(grepl("^ggcl-ring-", g$layout$name)))

  ## `position = "bottom"` (the default) draws the tree inside the hole, so it
  ## needs an inner radius.
  p_inner <- ggplot2::ggplot(long, ggplot2::aes(gene, sample, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_radial(theta = "x", inner.radius = 0.5) +
    scale_x_cluster(mat, method = "average", height = 1)
  g_inner <- ggplot2::ggplotGrob(p_inner)
  expect_true(any(grepl("^ggcl-ring-", g_inner$layout$name)))

  ## The categories are still re-ordered by the clustering.
  limits <- p$scales$get_scales("x")$limits
  expect_identical(limits, cluster_order(mat, "average"))
})


test_that("a faceted radial plot gets one ring tree per panel", {
  set.seed(22)
  make_panel <- function(panel, n) {
    m <- matrix(rnorm(n * 3), nrow = n,
                dimnames = list(paste0(panel, "_g", seq_len(n)), paste0("s", 1:3)))
    long <- as.data.frame(as.table(m))
    names(long) <- c("gene", "sample", "value")
    long$panel <- panel
    long
  }
  long <- rbind(make_panel("a", 8), make_panel("b", 6))
  p <- ggplot2::ggplot(long, ggplot2::aes(gene, sample, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~ panel, scales = "free_x") +
    ggplot2::coord_radial(theta = "x") +
    scale_x_cluster(method = "average", height = 0.8, position = "top")

  g <- ggplot2::ggplotGrob(p)
  rings <- grep("^ggcl-ring-", g$layout$name)
  expect_length(rings, 2L)
})


test_that("scale_y_cluster adds an r-axis tree on a coord_radial plot", {
  set.seed(23)
  mat <- matrix(rnorm(9 * 4), nrow = 9,
                dimnames = list(paste0("g", 1:9), paste0("s", 1:4)))
  long <- as.data.frame(as.table(mat))
  names(long) <- c("gene", "sample", "value")

  ## Full circle: the r labels sit in a column, where an arc tree makes no
  ## sense, so no r-axis tree is drawn.
  p <- ggplot2::ggplot(long, ggplot2::aes(gene, sample, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_radial(theta = "x", inner.radius = 0.5) +
    scale_y_cluster(t(mat), method = "average", height = 1)
  g <- ggplot2::ggplotGrob(p)
  expect_false(any(grepl("^ggcl-rtree-", g$layout$name)))

  ## Partial arc: the r labels sit on a radial ray and the tree becomes an
  ## arc-shaped wedge anchored on that ray.
  p_arc <- ggplot2::ggplot(long, ggplot2::aes(gene, sample, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_radial(theta = "x", start = -0.5 * pi, end = 0.5 * pi,
                          inner.radius = 0.5) +
    scale_y_cluster(t(mat), method = "average", height = 1)
  g_arc <- ggplot2::ggplotGrob(p_arc)
  expect_true(any(grepl("^ggcl-rtree-", g_arc$layout$name)))
})
