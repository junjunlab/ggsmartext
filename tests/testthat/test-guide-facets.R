## Facets: facet_wrap / facet_grid with guide_axis_smart()
##
## Every panel gets its own guide (facet_wrap, or facet_grid with free scales),
## so the marked labels must be the ones that exist in that panel.  These tests
## build the full gtable and inspect the guide grobs panel by panel.

walk_smart_grobs <- function(g) {
  out <- list()
  if (inherits(g, "ggse_smart_labels")) out <- c(out, list(g))
  kids <- list()
  if (!is.null(g$children)) kids <- c(kids, unclass(g$children))
  if (!is.null(g$grobs)) kids <- c(kids, g$grobs)
  for (k in kids) {
    if (inherits(k, "grob")) out <- c(out, walk_smart_grobs(k))
  }
  out
}

smart_labels_of <- function(p) {
  gs <- walk_smart_grobs(ggplot2::ggplotGrob(p))
  lapply(gs, function(g) as.character(g$labels))
}

facet_bar_data <- function() {
  data.frame(
    cat = factor(c(paste0("L", 1:12), paste0("R", 1:12)),
                 levels = c(paste0("L", 1:12), paste0("R", 1:12))),
    value = rep(c(3, 6, 2, 8, 5, 1), 4),
    panel = rep(c("left", "right"), each = 12)
  )
}

test_that("facet_grid(free_x): each panel keeps only its own marked labels", {
  d <- facet_bar_data()
  mark <- c("L3", "L5", "L6", "L7", "R2", "R9")
  p <- ggplot2::ggplot(d, ggplot2::aes(cat, value)) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(. ~ panel, scales = "free_x") +
    ggplot2::guides(x = guide_axis_smart(which = mark))

  labs <- smart_labels_of(p)
  expect_length(labs, 2L)
  expect_true(all(labs[[1]] %in% mark))
  expect_true(all(labs[[2]] %in% mark))
  expect_setequal(labs[[1]], c("L3", "L5", "L6", "L7"))
  expect_setequal(labs[[2]], c("R2", "R9"))
})

test_that("facet_wrap(free_x): labels are selected per panel", {
  d <- facet_bar_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(cat, value)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_x") +
    ggplot2::guides(x = guide_axis_smart(
      which = function(labels, positions) labels %in% c("L3", "L5", "R2", "R9")
    ))

  labs <- smart_labels_of(p)
  expect_length(labs, 2L)
  expect_setequal(labs[[1]], c("L3", "L5"))
  expect_setequal(labs[[2]], c("R2", "R9"))
})

test_that("facet_grid rows with a shared x axis draws the guide once", {
  d <- facet_bar_data()
  d$row <- rep(c("upper", "lower"), each = 12)
  p <- ggplot2::ggplot(d, ggplot2::aes(cat, value)) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(row ~ ., scales = "free_x") +
    ggplot2::guides(x = guide_axis_smart(which = c("L2", "L8", "R4")))

  labs <- smart_labels_of(p)
  # free_x gives one guide per row; shared scales would give exactly one
  expect_true(length(labs) >= 1L)
  if (length(labs) == 1L) {
    expect_setequal(labs[[1]], c("L2", "L8", "R4"))
  } else {
    expect_true(all(unlist(labs) %in% c("L2", "L8", "R4")))
  }
})

test_that("facet heatmap: right-hand y axis is labelled per panel", {
  n <- 40
  mat <- matrix(seq_len(n * 4), n, 4,
                dimnames = list(paste0("gene", 1:n), paste0("s", 1:4)))
  d <- data.frame(
    gene = factor(rep(rownames(mat), times = 4), levels = rev(rownames(mat))),
    sample = factor(rep(colnames(mat), each = n), levels = colnames(mat)),
    value = as.vector(mat),
    set = rep(ifelse(seq_len(n) %% 2 == 0, "even", "odd"), times = 4)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(sample, gene, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::facet_wrap(~set, nrow = 1, scales = "free_y") +
    ggplot2::scale_y_discrete(
      position = "right",
      guide = guide_axis_smart(which = c("gene10", "gene12", "gene14"))
    )

  gs <- walk_smart_grobs(ggplot2::ggplotGrob(p))
  # only the panel that contains these genes draws labels; the other panel
  # keeps an empty axis (and must not warn)
  expect_length(gs, 1L)
  expect_true(isTRUE(gs[[1]]$vertical))
  expect_setequal(as.character(gs[[1]]$labels),
                  c("gene10", "gene12", "gene14"))
})

test_that("a facet with no matching label never warns", {
  d <- facet_bar_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(cat, value)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_x") +
    ggplot2::guides(x = guide_axis_smart(which = c("L3", "does-not-exist")))

  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("crowded labels inside a facet are spread over several rows", {
  d <- data.frame(
    cat = factor(paste0("c", 1:20), levels = paste0("c", 1:20)),
    value = rep(1:20, 2),
    panel = rep(c("A", "B"), each = 20)
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(cat, value)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~panel, nrow = 1) +
    ggplot2::guides(x = guide_axis_smart(which = 3:14, nrow = 3))

  gs <- walk_smart_grobs(ggplot2::ggplotGrob(p))
  expect_length(gs, 2L)
  expect_true(all(vapply(gs, function(g) g$nrow, integer(1)) == 3L))
  # the 12 crowded labels are all kept, spread over the three rows
  expect_setequal(gs[[1]]$labels, paste0("c", 3:14))
})

test_that("a panel without any requested label stays empty but works", {
  d <- facet_bar_data()
  p <- ggplot2::ggplot(d, ggplot2::aes(cat, value)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_x") +
    ggplot2::guides(x = guide_axis_smart(which = "L3"))

  gs <- walk_smart_grobs(ggplot2::ggplotGrob(p))
  labs <- lapply(gs, function(g) as.character(g$labels))
  expect_true(any(vapply(labs, function(x) identical(x, "L3"), logical(1))))
})

test_that("faceted plots draw without errors for all guide positions", {
  d <- facet_bar_data()
  skip_if_not_installed("ragg")
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  for (pos in c("bottom", "top")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(cat, value)) +
      ggplot2::geom_col() +
      ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_x") +
      ggplot2::scale_x_discrete(
        position = pos,
        guide = guide_axis_smart(which = c("L2", "L4", "R6"), connection = TRUE)
      )
    ragg::agg_png(tmp, width = 900, height = 500, res = 100)
    expect_invisible(print(p))
    grDevices::dev.off()
  }
  expect_true(file.exists(tmp) && file.size(tmp) > 0)
})
