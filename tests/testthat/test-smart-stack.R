# smart_stack(): 多行标签排布引擎

test_that("返回结构与列类型符合契约", {
  res <- smart_stack(c(0.2, 0.5, 0.8), rep(0.1, 3), n = 2)
  expect_s3_class(res, "data.frame")
  expect_identical(names(res), c("index", "row", "start", "end", "center", "shown"))
  expect_identical(res$index, 1:3)
  expect_type(res$row, "integer")
  expect_type(res$start, "double")
  expect_type(res$end, "double")
  expect_type(res$center, "double")
  expect_type(res$shown, "logical")
})

test_that("空输入返回 0 行、列类型正确的 data.frame", {
  res <- smart_stack(numeric(0), numeric(0), n = 2)
  expect_equal(nrow(res), 0L)
  expect_identical(
    res,
    data.frame(
      index = integer(0), row = integer(0),
      start = numeric(0), end = numeric(0), center = numeric(0),
      shown = logical(0)
    )
  )
})

test_that("n = 1 且 padding = 0 时退化为 smart_align", {
  pos <- c(0.2, 0.22, 0.5)
  width <- rep(0.1, 3)
  res <- smart_stack(pos, width, n = 1)
  ref <- smart_align(pos - width / 2, pos + width / 2, c(0, 1))
  expect_equal(res$start, ref$start, tolerance = 1e-12)
  expect_equal(res$end, ref$end, tolerance = 1e-12)
  expect_true(all(res$shown))
})

test_that("平均分布的标签被均衡分到各行，几乎不移动", {
  pos <- seq(0.05, 0.95, length.out = 30)
  width <- rep(0.03, 30)
  res <- smart_stack(pos, width, n = 3)
  expect_true(all(res$shown))
  expect_equal(as.integer(table(res$row)), c(10L, 10L, 10L))
  expect_lt(max(abs(res$center - pos)), 1e-9)
  # 行内顺序与轴序一致
  for (r in 1:3) {
    expect_equal(res$index[res$row == r], seq(r, 30, by = 3))
  }
})

test_that("完全重叠的标签也能排开：行内不重叠、宽度守恒", {
  res <- smart_stack(rep(0.5, 4), rep(0.2, 4), n = 2)
  expect_true(all(res$shown))
  for (r in 1:2) {
    s <- sort(res$start[res$row == r])
    e <- sort(res$end[res$row == r])
    expect_true(all(s[-1] >= e[-length(e)] - 1e-12))
  }
  expect_equal(res$end - res$start, rep(0.2, 4), tolerance = 1e-12)
})

test_that("同一 cluster 内相邻标签的间隙恰为 padding", {
  res <- smart_stack(c(0.4, 0.5), rep(0.2, 2), n = 1, padding = 0.02)
  ord <- order(res$start)
  expect_equal(res$start[ord][2] - res$end[ord][1], 0.02, tolerance = 1e-12)
  # 盒子宽度不被压缩
  expect_equal(res$end - res$start, rep(0.2, 2), tolerance = 1e-12)
})

test_that("贴边的标签被夹回 range 内，extend 可放宽", {
  res <- smart_stack(c(0, 0.5, 1), rep(0.2, 3), n = 1)
  expect_equal(min(res$start), 0, tolerance = 1e-12)
  expect_equal(max(res$end), 1, tolerance = 1e-12)

  res <- smart_stack(c(0, 0.5, 1), rep(0.2, 3), n = 1, extend = 0.1)
  expect_equal(min(res$start), -0.1, tolerance = 1e-12)
  expect_equal(max(res$end), 1.1, tolerance = 1e-12)
})

test_that("容量不足时按 priority 丢弃低优先级标签", {
  pos <- rep(0.5, 5)
  width <- rep(0.4, 5)
  prio <- c(1, 5, 3, 4, 2)
  res <- smart_stack(pos, width, n = 1, priority = prio)
  # 一行最多放 2 个 0.4 宽的标签 -> 保留优先级最高的 2 个
  expect_equal(res$index[res$shown], c(2L, 4L))
  # 被丢弃的标签：row 为 NA、shown 为 FALSE、位置保持原始值
  dropped <- res[!res$shown, , drop = FALSE]
  expect_true(all(is.na(dropped$row)))
  expect_equal(dropped$center, pos[!res$shown])
  expect_equal(dropped$start, pos[!res$shown] - 0.2)
})

test_that("宽度超过整行可用宽度的标签被丢弃", {
  res <- smart_stack(c(0.2, 0.5, 0.8), c(0.1, 1.5, 0.1), n = 2)
  expect_false(res$shown[2])
  expect_true(is.na(res$row[2]))
  expect_true(all(res$shown[c(1, 3)]))
})

test_that("属性：行内不重叠、不越界、顺序保持、确定性（随机 200 组）", {
  set.seed(2024)
  for (i in seq_len(200)) {
    m <- sample(1:60, 1)
    pos <- stats::runif(m, 0.02, 0.98)
    width <- stats::runif(m, 0.005, 0.08)
    n <- sample(1:6, 1)
    pad <- sample(c(0, 0.002, 0.01), 1)
    res <- smart_stack(pos, width, n = n, padding = pad)
    shown <- res[res$shown, , drop = FALSE]

    if (nrow(shown)) {
      # 行号合法
      expect_true(all(!is.na(shown$row)))
      expect_gte(min(shown$row), 1L)
      expect_lte(max(shown$row), n)
      # 不越界（extend = 0）
      expect_gte(min(shown$start), -1e-9)
      expect_lte(max(shown$end), 1 + 1e-9)
      # 宽度守恒
      expect_equal(shown$end - shown$start, width[shown$index], tolerance = 1e-10)
      # 行内不重叠 + 间隙 + 顺序
      for (r in unique(shown$row)) {
        idx <- shown$index[shown$row == r]
        idx <- idx[order(res$center[idx])]
        if (length(idx) > 1) {
          expect_true(all(res$start[idx][-1] >= res$end[idx][-length(idx)] + pad - 1e-9))
          expect_false(is.unsorted(pos[idx]))
        }
      }
    }
    # 确定性
    expect_identical(smart_stack(pos, width, n = n, padding = pad), res)
  }
})

test_that("参数校验", {
  expect_error(smart_stack(1:3, 1:2, n = 2), "same length")
  expect_error(smart_stack(c(0.1, NA), c(0.1, 0.1), n = 2), "must not contain")
  expect_error(smart_stack(0.1, -0.1, n = 2), "negative")
  expect_error(smart_stack(0.1, 0.1, n = 0), "single integer")
  expect_error(smart_stack(0.1, 0.1, n = 2.5), "single integer")
  expect_error(smart_stack(0.1, 0.1, n = 2, extend = -0.1), "single finite number")
  expect_error(smart_stack(0.1, 0.1, n = 2, padding = -0.1), "single finite number")
  expect_error(smart_stack(0.1, 0.1, n = 2, priority = c(1, 2)), "same length as")
  expect_error(smart_stack(0.1, 0.1, n = 2, range = c(1, 0)), "increasing")
})
