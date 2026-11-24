# smart_align(): circlize::smartAlign 的移植

test_that("空输入返回 0 行的 data.frame", {
  res <- smart_align(numeric(0), numeric(0), c(0, 1))
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("start", "end"))
  expect_equal(nrow(res), 0L)
  expect_type(res$start, "double")
})

test_that("单元素：在范围内、范围外都原样返回（与参考实现一致）", {
  expect_equal(smart_align(0.5, 0.6, c(0, 1)), data.frame(start = 0.5, end = 0.6))
  expect_equal(smart_align(2, 3, c(0, 1)), data.frame(start = 2, end = 3))
})

test_that("不重叠（含首尾相接）的输入原样返回", {
  expect_equal(
    smart_align(c(0.1, 0.5, 0.9), c(0.2, 0.6, 1.0), c(0, 1)),
    data.frame(start = c(0.1, 0.5, 0.9), end = c(0.2, 0.6, 1.0))
  )
  # 首尾相接视为同一 cluster（参考实现的 box_overlap 语义），但位置不变
  expect_equal(
    smart_align(c(0.1, 0.2), c(0.2, 0.3), c(0, 1)),
    data.frame(start = c(0.1, 0.2), end = c(0.2, 0.3))
  )
})

test_that("轻微重叠被压成首尾相接，整体保持原始中点", {
  # 参考实现 circlize::smartAlign(c(0.10,0.12), c(0.14,0.16), c(0,1))
  res <- smart_align(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
  expect_equal(res$start, c(0.09, 0.13), tolerance = 1e-12)
  expect_equal(res$end, c(0.13, 0.17), tolerance = 1e-12)
})

test_that("全部完全重叠的极端情况：摊开后不重叠、居中", {
  # mid 取 (min start + max end)/2 = 0.55，总宽 0.4
  res <- smart_align(rep(0.5, 4), rep(0.6, 4), c(0, 1))
  expect_equal(res$start, c(0.35, 0.45, 0.55, 0.65), tolerance = 1e-12)
  expect_equal(res$end, c(0.45, 0.55, 0.65, 0.75), tolerance = 1e-12)
})

test_that("宽度之和超过 range 时居中溢出（与参考实现一致）", {
  # circlize::smartAlign() 在该分支把 cluster 居中，结果允许超出 range
  res <- smart_align(c(0.1, 0.2, 0.3), c(0.6, 0.7, 0.8), c(0, 1))
  expect_equal(res$start, c(-0.25, 0.25, 0.75), tolerance = 1e-12)
  expect_equal(res$end, c(0.25, 0.75, 1.25), tolerance = 1e-12)
})

test_that("与 circlize::smartAlign 逐元素一致（随机输入）", {
  skip_if_not_installed("circlize")
  set.seed(20241124)
  checked <- 0L
  for (i in seq_len(150)) {
    m <- sample(1:40, 1)
    start <- sort(stats::runif(m, 0, 0.9))
    width <- stats::runif(m, 0.001, 0.03)
    end <- start + width
    rng <- c(0, max(end) + stats::runif(1, 0.05, 0.3))
    if (sum(end - start) > diff(rng)) next
    ref <- circlize::smartAlign(start, end, rng)
    ours <- smart_align(start, end, rng)
    expect_equal(ours$start, ref$start, tolerance = 1e-10)
    expect_equal(ours$end, ref$end, tolerance = 1e-10)
    checked <- checked + 1L
  }
  expect_gt(checked, 100L)
})

test_that("可行输入下与 circlize::smartAlign 一致（range 固定为 0..1）", {
  skip_if_not_installed("circlize")
  set.seed(42)
  for (i in seq_len(60)) {
    m <- sample(2:20, 1)
    pos <- sort(stats::runif(m, 0.05, 0.95))
    width <- stats::runif(m, 0.002, 0.03)
    start <- pos - width / 2
    end <- pos + width / 2
    if (sum(width) > 1) next
    ref <- circlize::smartAlign(start, end, c(0, 1))
    ours <- smart_align(start, end, c(0, 1))
    expect_equal(ours$start, ref$start, tolerance = 1e-10)
    expect_equal(ours$end, ref$end, tolerance = 1e-10)
  }
})

test_that("属性：无重叠、顺序保持、不越界、idempotent（随机 200 组）", {
  set.seed(7)
  checked <- 0L
  for (i in seq_len(200)) {
    m <- sample(1:40, 1)
    pos <- sort(stats::runif(m, 0.05, 0.95))
    width <- stats::runif(m, 0.002, 0.03)
    start <- pos - width / 2
    end <- pos + width / 2
    if (sum(width) > 1) next
    res <- smart_align(start, end, c(0, 1))
    checked <- checked + 1L

    # 不重叠：相邻盒子 start[i] >= end[i-1]
    expect_true(all(res$start[-1] >= res$end[-length(res$end)] - 1e-12))
    # 顺序保持
    expect_equal(res$start, sort(res$start), tolerance = 1e-10)
    # 不越界
    expect_gte(min(res$start), -1e-12)
    expect_lte(max(res$end), 1 + 1e-12)
    # 宽度守恒
    expect_equal(res$end - res$start, width, tolerance = 1e-10)
    # idempotent
    again <- smart_align(res$start, res$end, c(0, 1))
    expect_equal(again$start, res$start, tolerance = 1e-12)
    expect_equal(again$end, res$end, tolerance = 1e-12)
  }
  expect_gt(checked, 150L)
})

test_that("参数校验", {
  expect_error(smart_align(1:3, 1:2, c(0, 1)), "same length")
  expect_error(smart_align(c(0.1, NA), c(0.2, 0.3), c(0, 1)), "must not contain")
  expect_error(smart_align(0.1, 0.2, c(1, 0)), "increasing")
  expect_error(smart_align(0.1, 0.2, 1), "length 2")
})
