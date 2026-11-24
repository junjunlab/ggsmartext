# box_arrange(): ComplexHeatmap::BoxArrange 的纯函数化移植

test_that("空输入返回 0 行的 data.frame", {
  res <- box_arrange(numeric(0), numeric(0), c(0, 1))
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("start", "end"))
  expect_equal(nrow(res), 0L)
})

test_that("单元素 / 不重叠输入：位置不变，行顺序与输入一致", {
  expect_equal(box_arrange(0.5, 0.6, c(0, 1)), data.frame(start = 0.5, end = 0.6))
  expect_equal(
    box_arrange(c(0.6, 0.1, 0.3), c(0.7, 0.2, 0.4), c(0, 1)),
    data.frame(start = c(0.6, 0.1, 0.3), end = c(0.7, 0.2, 0.4))
  )
})

test_that("乱序输入按中点处理，结果仍按输入顺序返回", {
  # 输入顺序：中间、最左、最右
  res <- box_arrange(c(0.5, 0.1, 0.9), c(0.52, 0.12, 0.92), c(0, 1))
  expect_equal(res, data.frame(start = c(0.5, 0.1, 0.9), end = c(0.52, 0.12, 0.92)))
})

test_that("轻微重叠被压成首尾相接（与参考实现同解）", {
  res <- box_arrange(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
  expect_equal(res$start, c(0.09, 0.13), tolerance = 1e-12)
  expect_equal(res$end, c(0.13, 0.17), tolerance = 1e-12)
})

test_that("中点完全重合时结果确定、不消耗随机数", {
  start <- c(0.45, 0.40, 0.42)
  end <- c(0.55, 0.60, 0.58) # 三个盒子的中点都是 0.50
  res1 <- box_arrange(start, end, c(0, 1))
  res2 <- box_arrange(start, end, c(0, 1))
  expect_identical(res1, res2)

  # 参考实现用 rank(ties.method = "random")，本实现必须是确定性的
  set.seed(1)
  expected_next <- stats::runif(1)
  set.seed(1)
  invisible(box_arrange(start, end, c(0, 1)))
  expect_identical(stats::runif(1), expected_next)

  # 中点重合、宽度相同的极端情况下，不重叠且整体居中（mid = 0.55）
  res <- box_arrange(rep(0.5, 4), rep(0.6, 4), c(0, 1))
  expect_equal(sort(res$start), c(0.35, 0.45, 0.55, 0.65), tolerance = 1e-12)
  expect_equal(sort(res$end), c(0.45, 0.55, 0.65, 0.75), tolerance = 1e-12)
})

test_that("与 ComplexHeatmap:::smartAlign2 逐元素一致（随机可行输入）", {
  skip_if_not_installed("ComplexHeatmap")
  set.seed(20241124)
  checked <- 0L
  for (i in seq_len(150)) {
    m <- sample(1:40, 1)
    start <- stats::runif(m, 0, 1)
    width <- stats::runif(m, 0.001, 0.04)
    end <- pmin(start + width, 1)
    start <- pmin(start, end)
    ref <- as.matrix(ComplexHeatmap:::smartAlign2(start, end, c(0, 1)))
    ours <- box_arrange(start, end, c(0, 1))
    expect_equal(ours$start, unname(ref[, 1]), tolerance = 1e-10)
    expect_equal(ours$end, unname(ref[, 2]), tolerance = 1e-10)
    checked <- checked + 1L
  }
  expect_gt(checked, 100L)
})

test_that("与 ComplexHeatmap:::smartAlign2 逐元素一致（宽度之和超 range）", {
  skip_if_not_installed("ComplexHeatmap")
  set.seed(99)
  checked <- 0L
  for (i in seq_len(100)) {
    m <- sample(2:10, 1)
    start <- stats::runif(m, 0, 0.5)
    width <- stats::runif(m, 0.2, 0.5)
    end <- start + width
    if (sum(width) <= 1) next
    ref <- as.matrix(ComplexHeatmap:::smartAlign2(start, end, c(0, 1)))
    ours <- box_arrange(start, end, c(0, 1))
    expect_equal(ours$start, unname(ref[, 1]), tolerance = 1e-10)
    expect_equal(ours$end, unname(ref[, 2]), tolerance = 1e-10)
    # 等比摊开：宽度守恒，最左（按中点）的盒子左边贴住 range[1]
    expect_equal(ours$end - ours$start, end - start, tolerance = 1e-10)
    leftmost <- order((start + end) / 2)[1]
    expect_equal(ours$start[leftmost], 0, tolerance = 1e-12)
    checked <- checked + 1L
  }
  expect_gt(checked, 50L)
})

test_that("属性：无重叠、不越界、宽度守恒、idempotent（随机 200 组）", {
  set.seed(7)
  checked <- 0L
  for (i in seq_len(200)) {
    m <- sample(1:40, 1)
    pos <- stats::runif(m, 0.02, 0.98)
    width <- stats::runif(m, 0.002, 0.04)
    start <- pmax(pos - width / 2, 0)
    end <- pmin(start + width, 1)
    if (sum(width) > 1) next
    res <- box_arrange(start, end, c(0, 1))
    checked <- checked + 1L

    # 行顺序与输入一致 -> 按 start 排序后再检查不重叠
    ord <- order(res$start, res$end)
    s <- res$start[ord]
    e <- res$end[ord]
    expect_true(all(s[-1] >= e[-length(e)] - 1e-9))
    expect_gte(min(res$start), -1e-12)
    expect_lte(max(res$end), 1 + 1e-12)
    expect_equal(res$end - res$start, end - start, tolerance = 1e-10)

    again <- box_arrange(res$start, res$end, c(0, 1))
    expect_equal(again$start, res$start, tolerance = 1e-9)
    expect_equal(again$end, res$end, tolerance = 1e-9)
  }
  expect_gt(checked, 150L)
})

test_that("参数校验", {
  expect_error(box_arrange(1:3, 1:2, c(0, 1)), "same length")
  expect_error(box_arrange(0.5, 0.4, c(0, 1)), "negative")
  expect_error(box_arrange(c(0.1, NA), c(0.2, 0.3), c(0, 1)), "must not contain")
  expect_error(box_arrange(0.1, 0.2, c(1, 0)), "increasing")
})
