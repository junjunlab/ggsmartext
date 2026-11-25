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
