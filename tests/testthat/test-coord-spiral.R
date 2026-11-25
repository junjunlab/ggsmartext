test_that("coord_spiral builds a coord and validates its arguments", {
  cs <- coord_spiral()
  expect_s3_class(cs, "CoordSpiral")
  expect_s3_class(cs, "Coord")

  expect_error(coord_spiral(start = 400, end = 200), "end > start")
  expect_error(coord_spiral(inner = 0.4, outer = 0.2), "inner")
  expect_error(coord_spiral(height = 2), "between 0 and 1")
  expect_error(coord_spiral(scale_by = "nope"), "arg")
})

test_that("transform bends data onto the spiral and respects the frame", {
  d <- data.frame(x = seq(0, 1, length.out = 50),
                  y = seq(0.2, 0.8, length.out = 50))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() + coord_spiral()
  b <- ggplot2::ggplot_build(p)

  ## all transformed positions stay in the panel and the radius grows with x
  fr <- ggutils:::.ggspiral_frame(b$layout$panel_params[[1]], b$layout$coord)
  xy <- ggutils:::.ggspiral_xy(d$x, d$y, fr)
  expect_true(all(is.finite(xy$x)))
  expect_true(all(is.finite(xy$y)))
  expect_true(all(xy$x >= 0 & xy$x <= 1))
  expect_true(all(xy$y >= 0 & xy$y <= 1))
  r <- sqrt((xy$x - 0.5)^2 + (xy$y - 0.5)^2)
  expect_true(all(diff(r) > -1e-8))
  expect_gt(tail(r, 1), head(r, 1))
})

test_that("all geoms and options render without warnings", {
  d <- data.frame(x = seq(0, 1, length.out = 80),
                  y = sin(seq(0, 1, length.out = 80) * 4 * pi) * 0.3 + 0.5)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_line() + coord_spiral(end = 360 + 90)
  expect_no_error(ggplot2::ggplotGrob(p))
  expect_no_error(ggplot2::ggplotGrob(
    p + ggplot2::geom_point(size = 0.5)
  ))
  expect_no_error(ggplot2::ggplotGrob(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      ggplot2::geom_area(fill = "grey70") +
      coord_spiral(scale_by = "curve", clockwise = TRUE)
  ))
  expect_no_error(ggplot2::ggplotGrob(
    ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      ggplot2::geom_line() +
      coord_spiral(reverse = TRUE, flip = "horizontal", labels = FALSE)
  ))
})

test_that("facet_wrap and facet_grid give every panel its own spiral", {
  d <- data.frame(x = rep(seq(0, 1, length.out = 40), 4),
                  y = runif(160),
                  a = rep(c("A", "B"), each = 80),
                  b = rep(rep(c("g1", "g2"), each = 40), 2))

  for (n_panels in c(2L, 4L)) {
    facet <- if (n_panels == 2L) ggplot2::facet_wrap(~a) else ggplot2::facet_grid(b ~ a)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      ggplot2::geom_line() + facet + coord_spiral(end = 360 + 90)
    g <- ggplot2::ggplotGrob(p)
    panels <- grep("^panel-", g$layout$name, value = TRUE)
    expect_length(panels, n_panels)
    expect_no_error(grid::grid.grabExpr(grid::grid.draw(g)))
  }

  ## free scales: each panel still lays out its own range
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~a, scales = "free_y") +
    coord_spiral(end = 360 + 90)
  b <- ggplot2::ggplot_build(p)
  expect_length(b$layout$panel_params, 2L)
  expect_false(identical(b$layout$panel_params[[1]]$y.range,
                         b$layout$panel_params[[2]]$y.range))
})
