## 边界行为回归测试（Lead 补充，对应 QA 报告的 P2 项）
## ggplot2 已在 setup.R 中加载。

test_that("`which` 为空向量时安全地不显示任何标签", {
  df <- data.frame(x = factor(letters[1:5]), y = 1:5)
  for (w in list(logical(0), integer(0), character(0))) {
    g <- guide_axis_smart(which = w)
    expect_s3_class(g, "ggproto")
    p <- ggplot(df, aes(x, y)) +
      geom_point() +
      guides(x = g)
    expect_silent(ggplot2::ggplotGrob(p))
  }
})

test_that("`which` 为空向量时内部选择结果全为 FALSE", {
  pos <- seq(0.1, 0.9, length.out = 4)
  sel <- ggutils:::.ggse_selection(logical(0), letters[1:4], pos)
  expect_identical(sel, rep(FALSE, 4))
  sel2 <- ggutils:::.ggse_selection(integer(0), letters[1:4], pos)
  expect_identical(sel2, rep(FALSE, 4))
  sel3 <- ggutils:::.ggse_selection(character(0), letters[1:4], pos)
  expect_identical(sel3, rep(FALSE, 4))
})

test_that("显式 nrow 太小且放不下时会提示标签会被丢弃", {
  df <- data.frame(x = factor(sprintf("S%02d", 1:40)), y = rnorm(40))
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    scale_x_discrete(guide = guide_axis_smart(nrow = 1))
  expect_message(ggplot2::ggplotGrob(p), "too small|not be drawn")
})

test_that("显式 nrow 足够时不会提示", {
  df <- data.frame(x = factor(sprintf("S%02d", 1:6)), y = rnorm(6))
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    scale_x_discrete(guide = guide_axis_smart(nrow = 2))
  expect_silent(ggplot2::ggplotGrob(p))
})
