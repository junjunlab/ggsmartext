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

test_that("scale_x/y_spiral_axis() hand their settings to the coord", {
  d <- data.frame(x = seq(0, 1, length.out = 40), y = seq(0, 1, length.out = 40))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    scale_x_spiral_axis(breaks = c(0.2, 0.8), label_size = 5) +
    scale_y_spiral_axis(breaks = c(0.3, 0.7), label_rot = 0,
                        tick_length = 0.03) +
    coord_spiral()
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  expect_equal(pp$spiral_axis$x$label_size, 5)
  expect_equal(pp$spiral_axis$x$tick_length, 0.007)
  expect_equal(pp$spiral_axis$y$tick_length, 0.03)
  expect_equal(pp$spiral_axis$y$label_rot, 0)
  expect_true(all(c(0.2, 0.8) %in% pp$x.major))
  expect_true(all(c(0.3, 0.7) %in% pp$y.major))
  expect_no_error(grid::grid.grabExpr(grid::grid.draw(ggplot2::ggplotGrob(p))))
})

.grob_kids <- function(g) if (inherits(g, "gtable")) g$grobs else g$children

.grob_names <- function(g) {
  kids <- .grob_kids(g)
  out <- names(kids)
  for (ch in kids) {
    if (inherits(ch, "gTree")) out <- c(out, .grob_names(ch))
  }
  out
}

.grob_find <- function(g, what) {
  kids <- .grob_kids(g)
  nm <- names(kids)
  for (i in seq_along(kids)) {
    if (identical(nm[i], what)) return(kids[[i]])
    if (inherits(kids[[i]], "gTree")) {
      r <- .grob_find(kids[[i]], what)
      if (!is.null(r)) return(r)
    }
  }
  NULL
}

test_that("scale_spiral_axis(show = FALSE) hides the matching axis", {
  d <- data.frame(x = seq(0, 1, length.out = 30), y = seq(0, 1, length.out = 30))
  base <- function() {
    ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  }

  g_all <- .grob_names(ggplot2::ggplotGrob(base() + coord_spiral()))
  expect_true(all(c("x_labels", "x_ticks", "y_labels", "y_ticks") %in% g_all))

  g_no_x <- .grob_names(ggplot2::ggplotGrob(
    base() + scale_x_spiral_axis(show = FALSE) + coord_spiral()))
  expect_false(any(c("x_labels", "x_ticks") %in% g_no_x))
  expect_true("y_labels" %in% g_no_x)

  g_no_y <- .grob_names(ggplot2::ggplotGrob(
    base() + scale_y_spiral_axis(show = FALSE) + coord_spiral()))
  expect_false(any(c("y_labels", "y_ticks") %in% g_no_y))
  expect_true("x_labels" %in% g_no_y)
})

test_that("axis_at puts the radial ruler where it is asked for", {
  d <- data.frame(x = seq(0, 1, length.out = 30), y = rep(0.6, 30))
  xy <- function(deg) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      scale_y_spiral_axis(axis_at = deg) + coord_spiral()
    b <- ggplot2::ggplot_build(p)
    ggutils:::.ggspiral_angle_to_theta(deg, ggutils:::.ggspiral_frame(
      b$layout$panel_params[[1]], b$layout$coord))
  }
  ## `clockwise`/`flip` are applied when drawing, so compare the *visual*
  ## direction: 0 deg must end up to the right of the centre
  visual <- function(deg) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      scale_y_spiral_axis(axis_at = deg) + coord_spiral()
    b <- ggplot2::ggplot_build(p)
    fr <- ggutils:::.ggspiral_frame(b$layout$panel_params[[1]], b$layout$coord)
    th <- ggutils:::.ggspiral_angle_to_theta(deg, fr)
    list(theta = th, fr = fr,
         sx = sin(th) * fr$dir * (if (isTRUE(fr$flip_h)) -1 else 1),
         sy = cos(th) * (if (isTRUE(fr$flip_v)) -1 else 1))
  }
  east <- visual(0)
  expect_equal(east$sx, 1, tolerance = 1e-8)
  expect_equal(east$sy, 0, tolerance = 1e-8)
  north <- visual(90)
  expect_equal(north$sx, 0, tolerance = 1e-8)
  expect_equal(north$sy, 1, tolerance = 1e-8)
  ## every angle lands inside the spiral, on its outermost turn
  for (deg in c(0, 45, 135, 270)) {
    v <- visual(deg)
    expect_gte(v$theta, v$fr$theta0 - 1e-8)
    expect_lte(v$theta, v$fr$theta1 + 1e-8)
    expect_gt(v$theta, v$fr$theta1 - 2 * pi - 1e-8)
  }
})

test_that("max_labels of scale_y_spiral_axis() thins the radial labels", {
  d <- data.frame(x = seq(0, 1, length.out = 30), y = seq(0, 1, length.out = 30))
  labels_of <- function(max_labels) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      scale_y_spiral_axis(max_labels = max_labels) + coord_spiral()
    g <- ggplot2::ggplotGrob(p)
    pan <- g$grobs[[which(g$layout$name == "panel")[1]]]
    gr <- .grob_find(pan, "y_labels")
    ## element_grob() returns a titleGrob, the text lives in its children
    txt <- if (!is.null(gr$label)) gr$label else
      unlist(lapply(gr$children, function(ch) ch$label))
    txt
  }
  ## an explicit `max_labels` is honoured exactly
  expect_equal(length(labels_of(2)), 2L)
  expect_equal(length(labels_of(4)), 4L)
  ## the default thins by track thickness, so it never floods a thin track
  expect_gte(length(labels_of(NULL)), 2L)
  expect_lte(length(labels_of(NULL)), 3L)
})

test_that("side puts the radial ruler at the start, the end or both", {
  d <- data.frame(x = seq(0, 1, length.out = 30), y = seq(0, 1, length.out = 30))
  n_rulers <- function(side) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      scale_y_spiral_axis(side = side) + coord_spiral()
    sum(grepl("^y_axis", .grob_names(ggplot2::ggplotGrob(p))))
  }
  expect_equal(n_rulers("end"), 1L)
  expect_equal(n_rulers("start"), 1L)
  expect_equal(n_rulers("both"), 2L)

  ## the labels are turned the same way at both sides and never upside down
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    scale_y_spiral_axis(side = "both", max_labels = 3) + coord_spiral()
  collect_rot <- function(g) {
    kids <- .grob_kids(g)
    nm <- names(kids)
    out <- numeric(0)
    for (i in seq_along(kids)) {
      ch <- kids[[i]]
      if (identical(nm[i], "y_labels")) {
        val <- if (!is.null(ch$rot)) ch$rot else
          unlist(lapply(ch$children, function(x) x$rot))
        out <- c(out, as.numeric(val))
      } else if (inherits(ch, "gTree")) {
        out <- c(out, collect_rot(ch))
      }
    }
    out
  }
  rots <- collect_rot(ggplot2::ggplotGrob(p))
  expect_gt(length(rots), 0L)
  expect_true(all(abs(rots) <= 90 + 1e-8))
})

test_that("turns and gap are shortcuts for end and height", {
  d <- data.frame(x = seq(0, 1, length.out = 20), y = seq(0, 1, length.out = 20))
  frame_of <- function(...) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      coord_spiral(...)
    b <- ggplot2::ggplot_build(p)
    ggutils:::.ggspiral_frame(b$layout$panel_params[[1]], b$layout$coord)
  }

  f2 <- frame_of(turns = 2)
  expect_equal((f2$theta1 - f2$theta0) / (2 * pi), 2, tolerance = 1e-8)

  f5 <- frame_of(turns = 5)
  expect_equal((f5$theta1 - f5$theta0) / (2 * pi), 5, tolerance = 1e-8)
  ## more turns on the same radius range means a thinner track and a tighter
  ## gap between the turns, which is what makes room for them
  expect_lt(f5$loop, f2$loop)
  expect_lt(f5$height, f2$height)

  ## gap is the complement of height
  fg <- frame_of(turns = 3, gap = 0.25)
  fh <- frame_of(turns = 3, height = 0.75)
  expect_equal(fg$height, fh$height, tolerance = 1e-12)

  expect_error(coord_spiral(turns = 0))
  expect_error(coord_spiral(gap = 1))
  expect_error(coord_spiral(gap = -0.1))
})

test_that("the layers used in the documentation all build on a spiral", {
  plots <- list(
    ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) + ggplot2::geom_point(),
    ggplot2::ggplot(economics, ggplot2::aes(date, unemploy)) + ggplot2::geom_line(),
    ggplot2::ggplot(economics, ggplot2::aes(date, unemploy)) + ggplot2::geom_area(),
    ggplot2::ggplot(mpg, ggplot2::aes(class, hwy)) + ggplot2::geom_col(),
    ggplot2::ggplot(mpg, ggplot2::aes(class)) + ggplot2::geom_bar(),
    ggplot2::ggplot(mpg, ggplot2::aes(hwy)) + ggplot2::geom_histogram(binwidth = 2),
    ggplot2::ggplot(mpg, ggplot2::aes(displ, hwy)) +
      ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm", formula = y ~ x),
    ggplot2::ggplot(mpg, ggplot2::aes(class, hwy)) + ggplot2::geom_boxplot(),
    ggplot2::ggplot(faithfuld, ggplot2::aes(waiting, eruptions, fill = density)) +
      ggplot2::geom_tile(),
    ggplot2::ggplot(mpg[seq(1, 234, 12), ], ggplot2::aes(displ, hwy, label = cyl)) +
      ggplot2::geom_text()
  )
  for (p in plots) {
    expect_no_error(ggplot2::ggplotGrob(p + coord_spiral(turns = 3)))
  }

  ## a matrix of values becomes stacked bars: three bands inside the track
  n <- 40
  m <- matrix(rep(1 / 3, 3 * n), ncol = 3)
  ds <- data.frame(x = rep(seq_len(n), 3),
                   stack = factor(rep(c("A", "B", "C"), each = n)),
                   v = as.vector(m))
  ps <- ggplot2::ggplot(ds, ggplot2::aes(x, v, fill = stack)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    coord_spiral(turns = 2, height = 0.8)
  b <- ggplot2::ggplot_build(ps)
  ## stacking keeps the total inside the track
  expect_true(all(b$data[[1]]$y <= 1 + 1e-8))
  expect_equal(max(b$data[[1]]$ymax), 1, tolerance = 1e-8)
  expect_no_error(grid::grid.grabExpr(grid::grid.draw(ggplot2::ggplotGrob(ps))))
})

test_that("segment and rect end points are warped onto the spiral", {
  d <- data.frame(x = c(0.1, 0.5), y = c(20, 80),
                  xend = c(0.2, 0.6), yend = c(40, 60))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_segment(ggplot2::aes(xend = xend, yend = yend)) +
    coord_spiral(turns = 2)
  b <- ggplot2::ggplot_build(p)
  tr <- b$layout$coord$transform(b$data[[1]], b$layout$panel_params[[1]])

  ## every end point has been moved into panel coordinates
  for (col in c("x", "y", "xend", "yend")) {
    expect_true(all(tr[[col]] >= 0 & tr[[col]] <= 1),
                info = col)
  }
  ## and it is really a *different* point from the start
  expect_true(all(abs(tr$xend - tr$x) > 1e-6))

  ## a rect keeps its corners inside the panel too
  dr <- data.frame(x = 0.3)
  pr <- ggplot2::ggplot(dr, ggplot2::aes(x)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = x, xmax = x + 0.1, ymin = 20, ymax = 60)) +
    coord_spiral(turns = 2)
  br <- ggplot2::ggplot_build(pr)
  trr <- br$layout$coord$transform(br$data[[1]], br$layout$panel_params[[1]])
  expect_true(all(trr$xmin >= 0 & trr$xmax <= 1))
  expect_true(all(trr$ymin >= 0 & trr$ymax <= 1))

  ## arrows do not break the drawing
  pa <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_segment(ggplot2::aes(xend = xend, yend = yend),
                          arrow = grid::arrow(length = grid::unit(2, "mm"))) +
    coord_spiral(turns = 2)
  expect_no_error(grid::grid.grabExpr(grid::grid.draw(ggplot2::ggplotGrob(pa))))
})

test_that("many points on a multi-turn spiral still draw their axis", {
  ## regression: the label-clearance test used to compare angles modulo 2*pi,
  ## which dropped labels a whole turn away and could empty the vector
  set.seed(1)
  for (n in c(200, 400, 1000)) {
    d <- data.frame(x = runif(n), y = runif(n))
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      coord_spiral(turns = 4, height = 0.85)
    expect_no_error(ggplot2::ggplotGrob(p))
  }
})

test_that("scale_x_spiral_axis(discrete = TRUE) works on a factor axis", {
  set.seed(3)
  d <- data.frame(g = factor(rep(sprintf("g%02d", 1:20), each = 12)),
                  v = rnorm(240))
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) +
    ggplot2::geom_violin(width = .9) +
    scale_x_spiral_axis(discrete = TRUE, breaks = sprintf("g%02d", seq(1, 20, 5))) +
    coord_spiral(turns = 2)
  expect_no_error(ggplot2::ggplotGrob(p))

  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  ## four labels out of twenty categories
  expect_length(pp$x.labels, 4L)
  expect_equal(pp$x.labels, sprintf("g%02d", seq(1, 20, 5)))

  ## boxplots on the same axis
  p2 <- ggplot2::ggplot(d, ggplot2::aes(g, v)) +
    ggplot2::geom_boxplot(width = .9) +
    scale_x_spiral_axis(discrete = TRUE) +
    coord_spiral(turns = 2)
  expect_no_error(ggplot2::ggplotGrob(p2))
})
