test_that("facet_gridn keeps one shared scale when scales = 'fixed'", {
  p <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
    ggplot2::geom_point() +
    facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = "fixed")
  lay <- ggplot2::ggplot_build(p)$layout$layout
  expect_true(all(lay$SCALE_X == 1L))
  expect_true(all(lay$SCALE_Y == 1L))
})

test_that("free_x_bycol / free_y_byrow keep the stock per-column / per-row scales", {
  base <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
    ggplot2::geom_point()

  bc <- ggplot2::ggplot_build(
    base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = "free_x_bycol")
  )$layout$layout
  expect_equal(bc$SCALE_X, bc$COL)
  expect_true(all(bc$SCALE_Y == 1L))

  br <- ggplot2::ggplot_build(
    base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = "free_y_byrow")
  )$layout$layout
  expect_true(all(br$SCALE_X == 1L))
  expect_equal(br$SCALE_Y, br$ROW)

  ## 这两个兼容档的刻度范围必须与 stock 完全一致
  rng <- function(p) {
    vapply(
      ggplot2::ggplot_build(p)$layout$panel_params,
      function(z) z$x$continuous_range[2], numeric(1)
    )
  }
  expect_equal(
    rng(base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year),
                           scales = "free_x_bycol")),
    rng(base + ggplot2::facet_grid(ggplot2::vars(drv), ggplot2::vars(year),
                                   scales = "free_x"))
  )
  expect_equal(
    rng(base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year),
                           scales = "free_y_byrow")),
    rng(base + ggplot2::facet_grid(ggplot2::vars(drv), ggplot2::vars(year),
                                   scales = "free_y"))
  )
})

test_that("compat modes only draw axes on the panel margins", {
  base <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
    ggplot2::geom_point()
  for (sc in c("fixed", "free_x_bycol", "free_y_byrow")) {
    g <- ggplot2::ggplotGrob(
      base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = sc)
    )
    expect_identical(sum(grepl("^axis-b", g$layout$name)), 2L)
    expect_identical(sum(grepl("^axis-l", g$layout$name)), 3L)
  }
})

test_that("facet_gridn assigns one scale per panel on the free axis", {
  base <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
    ggplot2::geom_point()

  fx <- ggplot2::ggplot_build(
    base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = "free_x")
  )$layout$layout
  expect_equal(fx$SCALE_X, as.integer(fx$PANEL))
  expect_true(all(fx$SCALE_Y == 1L))

  fy <- ggplot2::ggplot_build(
    base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = "free_y")
  )$layout$layout
  expect_true(all(fy$SCALE_X == 1L))
  expect_equal(fy$SCALE_Y, as.integer(fy$PANEL))

  fb <- ggplot2::ggplot_build(
    base + facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = "free")
  )$layout$layout
  expect_equal(fb$SCALE_X, as.integer(fb$PANEL))
  expect_equal(fb$SCALE_Y, as.integer(fb$PANEL))
})

test_that("facet_gridn frees ranges per panel where facet_grid frees per column", {
  build <- function(facet) {
    p <- ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
      ggplot2::geom_point() +
      facet(ggplot2::vars(drv), ggplot2::vars(year), scales = "free_x")
    b <- ggplot2::ggplot_build(p)
    vapply(b$layout$panel_params, function(z) z$x$continuous_range[2], numeric(1))
  }
  n_gridn <- length(unique(round(build(facet_gridn), 6)))
  n_stock <- length(unique(round(build(ggplot2::facet_grid), 6)))
  expect_identical(n_gridn, 6L)
  expect_identical(n_stock, 2L)
  expect_gt(n_gridn, n_stock)
})

test_that("facet_gridn draws its own axis for every panel when free", {
  p <- ggplot2::ggplot(mpg, ggplot2::aes(class, hwy)) +
    ggplot2::geom_boxplot() +
    facet_gridn(ggplot2::vars(drv), ggplot2::vars(year), scales = "free_x")
  g <- ggplot2::ggplotGrob(p)
  ## 自由维度每面板一条轴，共享维度只补外边距
  expect_identical(sum(grepl("^axis-b", g$layout$name)), 6L)
  expect_identical(sum(grepl("^axis-l", g$layout$name)), 3L)
})
