## coord_radial(): guide_axis_smart() must work on the theta axis (x or y)
##
## `coord_radial()` only accepts guides that know the `theta` aesthetic.  The
## guide draws through ggplot2's GuideAxisTheta, so the selected labels end up
## around the circle.  These tests check that the selection is applied and that
## the guide is no longer dropped with a "cannot be used for theta" warning.

collect_text_labels <- function(g) {
  out <- character()
  if (inherits(g, "text") && !is.null(g$label)) {
    out <- c(out, as.character(g$label))
  }
  kids <- list()
  if (!is.null(g$children)) kids <- c(kids, unclass(g$children))
  if (!is.null(g$grobs)) kids <- c(kids, g$grobs)
  for (k in kids) {
    if (inherits(k, "grob")) out <- c(out, collect_text_labels(k))
  }
  out
}

radial_labels <- function(p, universe) {
  labels <- collect_text_labels(ggplot2::ggplotGrob(p))
  sort(unique(labels[labels %in% universe]))
}

radial_data <- function() {
  data.frame(
    gene = factor(paste0("gene", 1:40), levels = paste0("gene", 1:40)),
    value = rep(c(3, 6, 2, 8, 5, 1), length.out = 40)
  )
}

test_that("coord_radial(theta = 'x'): only the selected labels are drawn", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x") +
    ggplot2::guides(theta = guide_axis_smart(which = seq(1, 40, by = 4)))

  expect_no_warning(ggplot2::ggplotGrob(p))
  labs <- radial_labels(p, levels(d$gene))
  expect_setequal(labs, paste0("gene", seq(1, 40, by = 4)))
})

test_that("coord_radial(theta = 'y'): n keeps n labels around the ring", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(value, gene)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "y") +
    ggplot2::guides(theta = guide_axis_smart(n = 10))

  labs <- radial_labels(p, levels(d$gene))
  expect_length(labs, 10L)
  expect_true(all(labs %in% levels(d$gene)))
  ## the thinning keeps the first and the last break
  expect_true("gene1" %in% labs)
  expect_true("gene40" %in% labs)
})

test_that("coord_radial: character `which` and tick = 'shown' work", {
  d <- radial_data()
  picked <- c("gene3", "gene12", "gene21", "gene30", "gene39")
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x") +
    ggplot2::guides(theta = guide_axis_smart(which = picked, tick = "shown"))

  labs <- radial_labels(p, levels(d$gene))
  expect_setequal(labs, picked)
})

test_that("coord_radial: an empty selection draws no labels but works", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x") +
    ggplot2::guides(theta = guide_axis_smart(which = character(0)))

  expect_no_warning(ggplot2::ggplotGrob(p))
  expect_length(radial_labels(p, levels(d$gene)), 0L)
})

test_that("coord_radial: the default ggplot2 theta guide is still used when asked", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x") +
    ggplot2::guides(theta = ggplot2::guide_axis_theta())

  labs <- radial_labels(p, levels(d$gene))
  expect_length(labs, 40L)
})

test_that("crowded theta labels are spread over rings and kept", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x") +
    ggplot2::guides(theta = guide_axis_smart(which = 10:21))

  expect_no_warning(ggplot2::ggplotGrob(p))
  labs <- radial_labels(p, levels(d$gene))
  expect_setequal(labs, paste0("gene", 10:21))
})

test_that(".ggse_theta_spread separates crowded labels and keeps the order", {
  pos <- c(0.30, 0.305, 0.31, 0.8)
  widths <- rep(0.02, 4)
  spread <- ggsmartext:::.ggse_theta_spread(pos, widths, wrap = TRUE)
  expect_true(all(spread$shown))
  ord <- order(spread$pos)
  starts <- spread$pos[ord] - widths[ord] / 2
  ends <- spread$pos[ord] + widths[ord] / 2
  expect_true(all(starts[-1] >= ends[-length(ends)] - 1e-9))
  ## the order is preserved: the first three stay together before the last one
  expect_lt(spread$pos[match(0.30, pos)], spread$pos[match(0.8, pos)])
})

test_that("crowded circular heatmap renders and labels the cluster", {
  data(airway_heatmap, package = "ggsmartext")
  d <- airway_heatmap$df
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, sample, fill = z)) +
    ggplot2::geom_tile() +
    ggplot2::coord_radial(theta = "x", inner.radius = 0.2) +
    ggplot2::guides(theta = guide_axis_smart(which = airway_heatmap$mark))

  expect_no_warning(ggplot2::ggplotGrob(p))
  labs <- radial_labels(p, levels(d$gene))
  expect_true(length(labs) >= 15L)
  expect_true(all(labs %in% airway_heatmap$mark))
})
test_that("half circle: crowded labels stay on the arc and are kept", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x", start = -0.5 * pi, end = 0.5 * pi) +
    ggplot2::guides(theta = guide_axis_smart(which = 10:21))

  expect_no_warning(ggplot2::ggplotGrob(p))
  labs <- radial_labels(p, levels(d$gene))
  expect_setequal(labs, paste0("gene", 10:21))
})

test_that("half circle: theta taken from the y scale works too", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(value, gene)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "y", start = -0.5 * pi, end = 0.5 * pi) +
    ggplot2::guides(theta = guide_axis_smart(which = c("gene2", "gene9", "gene18")))

  expect_no_warning(ggplot2::ggplotGrob(p))
  labs <- radial_labels(p, levels(d$gene))
  expect_setequal(labs, c("gene2", "gene9", "gene18"))
})

test_that("half-circle heatmap with a crowded cluster renders", {
  data(airway_heatmap, package = "ggsmartext")
  d <- airway_heatmap$df
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, sample, fill = z)) +
    ggplot2::geom_tile() +
    ggplot2::coord_radial(theta = "x", start = -0.5 * pi, end = 0.5 * pi,
                          inner.radius = 0.2) +
    ggplot2::guides(theta = guide_axis_smart(which = airway_heatmap$mark))

  expect_no_warning(ggplot2::ggplotGrob(p))
  labs <- radial_labels(p, levels(d$gene))
  expect_true(length(labs) >= 5L)
  expect_true(all(labs %in% airway_heatmap$mark))
})

test_that("radial style: labels are rotated along the radius", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x") +
    ggplot2::guides(theta = guide_axis_smart(which = 1:4))
  expect_no_warning(ggplot2::ggplotGrob(p))

  walk <- function(x) {
    out <- list()
    if (inherits(x, "text") && !is.null(x$label)) out <- c(out, list(x))
    kids <- list()
    if (!is.null(x$children)) kids <- c(kids, unclass(x$children))
    if (!is.null(x$grobs)) kids <- c(kids, x$grobs)
    for (k in kids) if (inherits(k, "grob")) out <- c(out, walk(k))
    out
  }
  gs <- walk(ggplot2::ggplotGrob(p))
  labs <- unlist(lapply(gs, function(x) as.character(x$label)))
  rots <- unlist(lapply(gs, function(x) as.numeric(x$rot)))
  keep <- grepl("^gene", labs)
  expect_true(all(c("gene1", "gene2", "gene3", "gene4") %in% labs[keep]))
  ## radial text at the sides of the circle is nearly vertical
  expect_true(any(abs(abs(rots[keep]) - 90) < 1))
})
test_that("radial labels inherit the axis text theme", {
  d <- radial_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(gene, value)) +
    ggplot2::geom_col() +
    ggplot2::coord_radial(theta = "x") +
    ggplot2::guides(theta = guide_axis_smart(which = c("gene1", "gene5"))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      size = 20, colour = "red", face = "bold"
    ))
  g <- ggplot2::ggplotGrob(p)
  walk <- function(x) {
    out <- list()
    if (inherits(x, "text") && !is.null(x$label)) out <- c(out, list(x))
    kids <- list()
    if (!is.null(x$children)) kids <- c(kids, unclass(x$children))
    if (!is.null(x$grobs)) kids <- c(kids, x$grobs)
    for (k in kids) if (inherits(k, "grob")) out <- c(out, walk(k))
    out
  }
  gs <- walk(g)
  labs <- vapply(gs, function(x) paste(as.character(x$label), collapse = ","), "")
  ## the guide draws one text grob holding all labels; the axis title "gene"
  ## is a different text grob
  idx <- which(grepl("gene1", labs) & grepl("gene5", labs))
  expect_true(length(idx) >= 1L)
  gps <- lapply(gs[idx], function(x) x$gp)
  expect_true(all(vapply(gps, function(gp) identical(gp$fontsize, 20), logical(1))))
  expect_true(all(vapply(gps, function(gp) identical(gp$col, "red"), logical(1))))
})