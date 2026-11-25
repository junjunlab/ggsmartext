# guide_axis_smart(): 集成测试
#
# 覆盖：两种挂载方式、which 四种写法、n 截断 / nrow 强制 / tick 三种模式 /
# connection 开关、参数校验、不重叠不越界属性、旋转标签预留高度、sec_axis、
# facet、连续轴以及各种边界输入。
#
# 说明：`.ggse_prepare()` / `smart_stack()` / `.ggse_label_grob()` 都是包内部
# 函数；testthat 的 test_check() 让测试代码在包命名空间里求值，因此可以直接
# 调用（和算法测试的写法保持一致）。

# ---- 内部工具 --------------------------------------------------------------

# 构造 `ggplot_build()` 阶段交给 guide 的 params 列表（`setup_params()` 会补出
# aes / vertical / position / opposite / lab_first 等字段）。
smart_params <- function(aes = "x", position = "bottom", ...) {
  guide <- guide_axis_smart()
  params <- utils::modifyList(guide$params, list(...))
  params$position <- position
  guide$setup_params(params)
}

# `calc_element()` 之后的主题元素组。
smart_elements <- function(params, theme = ggplot2::theme_grey()) {
  guide <- guide_axis_smart()
  guide$setup_elements(params, guide$elements, theme)
}

# 构造 guide 的 `key`：每行一个 break，位置列已经是 npc（0..1）。
smart_key <- function(labels, pos = NULL, aes = "x") {
  labels <- as.character(labels)
  if (is.null(pos)) {
    pos <- discrete_pos(length(labels))
  }
  key <- data.frame(pos = pos, .value = labels, .label = labels,
                    stringsAsFactors = FALSE)
  names(key)[1] <- aes
  key
}

# 直接跑一遍 `GuideAxisSmart$build_labels()` 之前的准备阶段。
smart_prepare <- function(labels, pos = NULL, aes = "x", position = "bottom",
                          theme = ggplot2::theme_grey(), ...) {
  params <- smart_params(aes = aes, position = position, ...)
  elements <- smart_elements(params, theme = theme)
  key <- smart_key(labels, pos = pos, aes = aes)
  list(
    key = key,
    params = params,
    elements = elements,
    prep = .ggse_prepare(key, params, elements)
  )
}

# ggplot2 离散轴的 npc 位置：数据 1..n 在 expansion 0.6 之后位于 c(0.4, n+0.6)。
discrete_pos <- function(n) {
  (seq_len(n) - 0.4) / (n + 0.2)
}

# 包内部估计面板尺寸时用的启发式，测试里保持一致，便于和 `smart_stack()` 对齐。
panel_cm <- function(vertical = FALSE) {
  0.7 * grDevices::dev.size("cm")[if (vertical) 2L else 1L]
}

# 从 ggplotGrob() 的 gtable 里递归找出智能标签 grob。
find_label_grob <- function(grob) {
  if (inherits(grob, "ggse_smart_labels")) {
    return(grob)
  }
  kids <- NULL
  if (!is.null(grob$grobs)) kids <- grob$grobs
  if (!is.null(grob$children)) kids <- c(kids, grob$children)
  if (is.null(kids) || length(kids) == 0L) {
    return(NULL)
  }
  for (kid in kids) {
    found <- find_label_grob(kid)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

# 在给定视口尺寸下触发 `makeContent()`，得到真正要画的子 grob。
draw_label_grob <- function(grob, width_cm, height_cm) {
  grid::pushViewport(grid::viewport(
    width  = grid::unit(width_cm, "cm"),
    height = grid::unit(height_cm, "cm")
  ))
  on.exit(grid::popViewport(), add = TRUE)
  grid::makeContent(grob)
}

# 子 grob 的 x/y 位置转成 npc 数值。
grob_x_npc <- function(grobs) {
  vapply(grobs, function(g) {
    grid::convertX(g$x, "npc", valueOnly = TRUE)[1L]
  }, numeric(1))
}

grob_y_npc <- function(grobs) {
  vapply(grobs, function(g) {
    grid::convertY(g$y, "npc", valueOnly = TRUE)[1L]
  }, numeric(1))
}

# 按 `smart_stack()` 的调用方式（= `makeContent()` 里的方式）重算一次布局。
layout_from_prep <- function(prep, panel = NULL) {
  vertical <- isTRUE(prep$vertical)
  if (is.null(panel)) {
    panel <- panel_cm(vertical)
  }
  shown <- prep$shown
  lay <- smart_stack(
    positions = prep$pos[shown],
    widths    = prep$box_along_cm[shown] / panel,
    n         = prep$nrow,
    range     = c(0, 1) + c(-prep$extend, prep$extend),
    padding   = prep$pad_cm / panel
  )
  list(lay = lay, panel = panel, shown = shown)
}

# 一个 40 类的示例数据；`g` 是字符 -> 因子水平按字典序，恰好也检验了
# "标签顺序不乱"（位置顺序才是轴顺序）。
example_data <- function(n = 40) {
  data.frame(
    g = factor(paste0("S", seq_len(n))),
    v = seq_len(n) / n,
    stringsAsFactors = FALSE
  )
}

# ---- 1. 两种挂载方式 -------------------------------------------------------

test_that("guides() 与 scale(guide=) 两种挂载方式在 x、y 轴都出图且不报错", {
  d <- example_data(40)

  plots <- list(
    guides_x   = ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::guides(x = guide_axis_smart(n = 8)),
    scale_x    = ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(n = 8)),
    guides_y   = ggplot2::ggplot(d, ggplot2::aes(v, g)) + ggplot2::geom_point() +
      ggplot2::guides(y = guide_axis_smart(n = 8)),
    scale_y    = ggplot2::ggplot(d, ggplot2::aes(v, g)) + ggplot2::geom_point() +
      ggplot2::scale_y_discrete(guide = guide_axis_smart(n = 8))
  )

  for (nm in names(plots)) {
    p <- plots[[nm]]
    expect_silent(gt <- ggplot2::ggplotGrob(p))
    expect_s3_class(gt, "gtable")

    # 确实生成了智能标签 grob，而且 n = 8 生效
    lg <- find_label_grob(gt)
    expect_false(is.null(lg), info = nm)
    expect_length(lg$labels, 8L)
  }

  # 完整画一遍（makeContent() 也会跑到）
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)
  expect_no_error(
    ggplot2::ggsave(out, plot = plots$scale_x, width = 7, height = 5, dpi = 72)
  )
  expect_true(file.exists(out))
})

# ---- 2. which 四种写法 -----------------------------------------------------

test_that("which 的整数 / 字符 / 逻辑 / 函数四种写法都能正确选择标签", {
  labels <- paste0("S", seq_len(20))
  pos <- discrete_pos(20)

  shown_index <- function(...) {
    which(smart_prepare(labels, pos = pos, ...)$prep$shown)
  }

  expect_equal(shown_index(which = c(2L, 5L, 9L)), c(2L, 5L, 9L))
  expect_equal(shown_index(which = c("S2", "S10")), c(2L, 10L))
  expect_equal(
    shown_index(which = seq_along(labels) %% 2L == 0L),
    which(seq_along(labels) %% 2L == 0L)
  )
  expect_equal(
    shown_index(which = function(lab, positions) positions > 0.5),
    11:20
  )
  expect_equal(
    shown_index(which = function(lab, positions) c(1L, 3L, 5L)),
    c(1L, 3L, 5L)
  )

  # 端到端：标签 grob 里实际显示的标签集合与选择一致
  d <- example_data(20)
  show_via_plot <- function(which) {
    p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(which = which))
    lg <- find_label_grob(ggplot2::ggplotGrob(p))
    lg$labels
  }
  expect_equal(
    sort(show_via_plot(c("S2", "S10")), method = "radix"),
    sort(labels[c(2L, 10L)], method = "radix")
  )
  # `d$g` 的因子水平按字典序（S1, S10, ..., S2, ...），轴位置顺序 = 因子水平
  # 顺序，所以端到端的期望值要用 levels() 取。
  lev <- levels(d$g)
  expect_equal(
    sort(show_via_plot(function(lab, positions) positions > 0.5), method = "radix"),
    sort(lev[11:20], method = "radix")
  )
  expect_equal(show_via_plot(c(1L, 3L, 5L)), lev[c(1L, 3L, 5L)])
})

test_that("which 的退化/非法输入有明确的报错或警告", {
  d <- example_data(12)

  # 逻辑向量长度与 break 数不符：绘制阶段报错
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart(which = TRUE))
  expect_error(ggplot2::ggplotGrob(p), "same length")

  # 数字下标越界
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart(which = 999))
  expect_error(ggplot2::ggplotGrob(p), "between 1 and 12")

  # which 函数返回逻辑向量但长度不对
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart(which = function(lab, pos) TRUE))
  expect_error(ggplot2::ggplotGrob(p), "length")

  # which 函数返回不支持的类型
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart(which = function(lab, pos) "S1"))
  expect_error(ggplot2::ggplotGrob(p), "logical or numeric")

  # 字符 which 里有没有的标签：警告但不报错
  # character `which`: labels that are not on the axis are skipped silently
  # (faceted plots request one shared `which` for all panels)
  expect_no_warning(
    smart_prepare(paste0("S", 1:5), which = c("x1", "not-there"))
  )
  expect_no_warning(
    smart_prepare(paste0("S", 1:5), which = c("S2", "not-there"))
  )
})

# ---- 3. n / nrow / tick / connection --------------------------------------

test_that("n 截断与 which 组合时最多显示 n 个标签", {
  labels <- paste0("S", seq_len(20))

  res <- smart_prepare(labels, which = NULL, n = 5)
  idx <- which(res$prep$shown)
  expect_length(idx, 5L)
  # 等距保留，首尾都在
  expect_true(1L %in% idx)
  expect_true(20L %in% idx)

  res <- smart_prepare(labels, which = 1:10, n = 4)
  idx <- which(res$prep$shown)
  expect_length(idx, 4L)
  expect_true(all(idx %in% 1:10))

  # n 大于标签数时全部显示
  res <- smart_prepare(labels, n = 100)
  expect_equal(sum(res$prep$shown), 20L)

  # 端到端确认
  d <- example_data(40)
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart(n = 6))
  lg <- find_label_grob(ggplot2::ggplotGrob(p))
  expect_length(lg$labels, 6L)
})

test_that("nrow 强制轨道数，并受标签总数与 max_nrow 约束", {
  labels <- paste0("S", seq_len(20))

  expect_equal(smart_prepare(labels, nrow = 3)$prep$nrow, 3L)
  expect_equal(smart_prepare(labels, nrow = 1)$prep$nrow, 1L)
  # 不能多于实际显示标签数
  expect_equal(smart_prepare(labels, nrow = 50)$prep$nrow, 20L)

  # 自动轨道数不超过 max_nrow
  auto <- smart_prepare(labels)
  expect_lte(auto$prep$nrow, 8L)
  capped <- smart_prepare(labels, max_nrow = 2)
  expect_lte(capped$prep$nrow, 2L)
  # 显式 nrow 也不应超过 max_nrow 之外的容量约束（nrow 是用户直接要求，
  # 这里只检查它被保留）
  expect_equal(smart_prepare(labels, nrow = 3, max_nrow = 2)$prep$nrow, 3L)

  # panel_width 越小，自动轨道数不会更少
  wide  <- smart_prepare(paste0("S", seq_len(80)), panel_width = 30)$prep$nrow
  narrow <- smart_prepare(paste0("S", seq_len(80)), panel_width = 4)$prep$nrow
  expect_gte(narrow, wide)
})

test_that("tick 三种模式分别控制刻度：all / shown / none", {
  build_ticks_for <- function(tick, which = NULL) {
    guide <- guide_axis_smart(tick = tick, which = which)
    params <- guide$params
    params$position <- "bottom"
    params <- guide$setup_params(params)
    elements <- guide$setup_elements(params, guide$elements, ggplot2::theme_grey())
    key <- smart_key(paste0("S", seq_len(10)))
    list(
      grob = guide$build_ticks(key, elements, params),
      params = params,
      elements = elements,
      key = key
    )
  }

  all_ticks <- build_ticks_for("all")
  # 刻度 grob 是 polyline，每个刻度两个端点
  expect_equal(length(all_ticks$grob$x), 20L)

  shown_ticks <- build_ticks_for("shown", which = c(1L, 4L, 7L, 10L))
  expect_equal(length(shown_ticks$grob$x), 8L)
  expect_equal(
    grid::convertX(shown_ticks$grob$x, "npc", valueOnly = TRUE),
    rep(discrete_pos(10)[c(1L, 4L, 7L, 10L)], each = 2L)
  )

  none_ticks <- build_ticks_for("none")
  expect_s3_class(none_ticks$grob, "zeroGrob")

  # 端到端三种模式都能画
  d <- example_data(20)
  for (tm in c("all", "shown", "none")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(tick = tm, n = 6))
    expect_silent(ggplot2::ggplotGrob(p))
  }
})

test_that("connection = FALSE 时不画引导线，连接线外观参数可生效", {
  labels <- paste0("S", seq_len(10))
  pos <- discrete_pos(10)

  prep <- smart_prepare(labels, pos = pos, connection = FALSE)$prep
  expect_false(prep$connection)
  lg <- .ggse_label_grob(prep, smart_params(connection = FALSE))
  expect_false(lg$connection)

  rendered <- draw_label_grob(lg, panel_cm(), prep$total_across_cm)
  classes <- vapply(rendered$children, function(g) class(g)[1L], character(1))
  expect_false(any(classes == "segments"))
  expect_equal(sum(classes == "text"), 10L)

  # 默认画 3 段折线 / 标签（anno_mark 风格）
  prep2 <- smart_prepare(labels, pos = pos)$prep
  lg2 <- .ggse_label_grob(prep2, smart_params())
  rendered2 <- draw_label_grob(lg2, panel_cm(), prep2$total_across_cm)
  seg <- rendered2$children[[1L]]
  expect_s3_class(seg, "segments")
  expect_equal(length(seg$x0), 3L * 10L)

  # 线色 / 线宽 / 线型
  params3 <- smart_params(connection_col = "red", connection_lwd = 2, connection_lty = 2)
  prep3 <- .ggse_prepare(smart_key(labels, pos), params3, smart_elements(params3))
  expect_identical(prep3$connection_col, "red")
  lg3 <- .ggse_label_grob(prep3, params3)
  rendered3 <- draw_label_grob(lg3, panel_cm(), prep3$total_across_cm)
  seg3 <- rendered3$children[[1L]]
  expect_equal(seg3$gp$col, "red")
  expect_equal(seg3$gp$lwd, 2)
  expect_equal(seg3$gp$lty, 2)
})

test_that("connection_style 三种样式都画出预期的折线段数", {
  labels <- paste0("S", seq_len(10))
  pos <- discrete_pos(10)

  for (style in c("diagonal", "straight", "elbow")) {
    params <- smart_params(connection_style = style)
    prep <- .ggse_prepare(smart_key(labels, pos), params, smart_elements(params))
    lg <- .ggse_label_grob(prep, params)
    rendered <- draw_label_grob(lg, panel_cm(), prep$total_across_cm)
    seg <- rendered$children[[1L]]
    expected_segments <- if (identical(style, "straight")) 1L else 3L
    expect_equal(length(seg$x0), expected_segments * 10L, info = style)
    expect_equal(sum(vapply(rendered$children, function(g) {
      inherits(g, "text")
    }, logical(1))), 10L, info = style)
  }
})

# ---- 4. 参数校验 -----------------------------------------------------------

test_that("guide_axis_smart() 对非法参数给出清晰报错", {
  expect_error(guide_axis_smart(which = list(1)), "which")
  expect_error(guide_axis_smart(n = 0), "n")
  expect_error(guide_axis_smart(n = -1), "n")
  expect_error(guide_axis_smart(nrow = 0), "nrow")
  expect_error(guide_axis_smart(nrow = -2), "nrow")
  expect_error(guide_axis_smart(padding = -0.1), "padding")
  expect_error(guide_axis_smart(padding = "a"), "padding")
  expect_error(guide_axis_smart(gap = -1), "gap")
  expect_error(guide_axis_smart(extend = -0.1), "extend")
  expect_error(guide_axis_smart(extend = 0.9), "extend")
  expect_error(guide_axis_smart(connection = NA), "connection")
  expect_error(guide_axis_smart(connection = "yes"), "connection")
  expect_error(guide_axis_smart(connection_lwd = -1), "connection_lwd")
  expect_error(guide_axis_smart(link_length = -1), "link_length")
  expect_error(guide_axis_smart(panel_width = 0), "panel_width")
  expect_error(guide_axis_smart(max_nrow = 0), "max_nrow")
  expect_error(guide_axis_smart(hjust = 2), "hjust")
  expect_error(guide_axis_smart(vjust = -1), "vjust")
  expect_error(guide_axis_smart(connection_style = "wavy"), "should be one of")
  expect_error(guide_axis_smart(tick = "some"), "should be one of")
  expect_error(guide_axis_smart(cap = "upper-and-lower"), "should be one of")
})

test_that("合法边界参数可以构造并通过绘制", {
  d <- example_data(20)
  plots <- list(
    ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(extend = 0.5)),
    ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(padding = 0, gap = 0)),
    ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(link_length = 0,
                                                         connection = FALSE)),
    ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(cap = "both",
                                                         hjust = 0, vjust = 1))
  )
  for (p in plots) {
    expect_silent(ggplot2::ggplotGrob(p))
  }
})

# ---- 5. 同轨道不重叠、不越界（属性测试）-----------------------------------

test_that("同轨道标签盒不重叠，extend = 0 时全部落在 [0, 1] 内", {
  scenario <- function(n, nrow = NULL, aes = "x", position = "bottom",
                       extend = 0, panel_width = NULL) {
    labels <- paste0("S", seq_len(n))
    pos <- discrete_pos(n)
    res <- smart_prepare(labels, pos = pos, aes = aes, position = position,
                         nrow = nrow, extend = extend,
                         panel_width = panel_width)
    prep <- res$prep
    expect_false(is.null(prep))
    vertical <- isTRUE(prep$vertical)
    panel <- if (is.null(panel_width)) panel_cm(vertical) else panel_width
    out <- layout_from_prep(prep, panel = panel)
    lay <- out$lay
    expect_equal(nrow(lay), sum(prep$shown))

    lim <- c(0, 1) + c(-extend, extend)
    for (r in unique(lay$row[lay$shown])) {
      part <- lay[lay$shown & lay$row == r, , drop = FALSE]
      part <- part[order(part$start), , drop = FALSE]
      if (nrow(part) > 1L) {
        # 同一轨道内按轴序排布，相邻标签盒不重叠
        expect_true(
          all(part$start[-1L] >= part$end[-nrow(part)] - 1e-12),
          info = sprintf("n=%d row=%d", n, r)
        )
      }
      expect_true(
        all(part$start >= lim[1L] - 1e-9 & part$end <= lim[2L] + 1e-9),
        info = sprintf("n=%d row=%d", n, r)
      )
    }
    # 不同轨道之间在"横向"方向也不重叠：总预留高度足够
    if (any(prep$shown)) {
      expect_gte(
        prep$total_across_cm,
        prep$nrow * max(prep$box_across_cm[prep$shown])
      )
    }
    invisible(list(lay = lay, prep = prep))
  }

  # 自动轨道（含 40/50/200 个分类）
  invisible(scenario(20))
  invisible(scenario(40))
  invisible(scenario(50))
  invisible(scenario(200))
  # 强制轨道
  invisible(scenario(40, nrow = 1))
  invisible(scenario(40, nrow = 3))
  invisible(scenario(50, nrow = 6))
  # y 轴（列方向）
  invisible(scenario(30, aes = "y", position = "left"))
  invisible(scenario(30, aes = "y", position = "left", nrow = 3))
  # extend > 0 时允许越界，但结果仍落在放宽后的范围内
  invisible(scenario(20, nrow = 4, extend = 0.1))
})

test_that("渲染后的标签盒也不重叠、不越界（40 类 x 轴，两行）", {
  d <- example_data(40)
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart(nrow = 2))
  lg <- find_label_grob(ggplot2::ggplotGrob(p))

  panel <- panel_cm(FALSE)
  across_cm <- attr(lg, "ggse_size_cm")
  rendered <- draw_label_grob(lg, panel, across_cm)

  seg <- rendered$children[[1L]]
  texts <- rendered$children[-1L]
  tx <- grob_x_npc(texts)
  ty <- grob_y_npc(texts)
  expect_length(tx, length(lg$labels))

  # 渲染位置必须和 makeContent() 里用同样的输入调用 smart_stack() 得到的
  # 精确布局一致（即"算出来的位置"和"画出来的位置"是同一份）。
  lay <- smart_stack(
    positions = lg$pos,
    widths    = lg$box_along / panel,
    n         = lg$nrow,
    range     = c(0, 1),
    padding   = lg$pad_cm / panel
  )
  keep <- as.logical(lay$shown)
  expect_equal(sum(keep), length(texts))
  expect_equal(unname(tx), lay$center[keep], tolerance = 1e-9)

  # 文字锚点与标签盒都在视口内
  box_along_npc <- lg$box_along / panel
  box_across_npc <- lg$box_across / across_cm
  expect_true(all(tx - box_along_npc / 2 >= -1e-9))
  expect_true(all(tx + box_along_npc / 2 <= 1 + 1e-9))
  expect_true(all(ty >= -1e-9 & ty <= 1 + 1e-9))
  expect_true(all(ty - box_across_npc >= -1e-9))

  # 引导线：从原始刻度位置出发，收在标签中心
  expect_equal(length(seg$x0), 3L * length(texts))
  first <- seq(1L, by = 3L, length.out = length(texts))
  last <- first + 2L
  expect_equal(
    unname(grid::convertX(seg$x0[first], "npc", valueOnly = TRUE)),
    lg$pos[keep],
    tolerance = 1e-9
  )
  expect_true(all(grid::convertY(seg$y0[first], "npc", valueOnly = TRUE) == 1))
  expect_equal(
    unname(grid::convertX(seg$x1[last], "npc", valueOnly = TRUE)),
    unname(tx),
    tolerance = 1e-9
  )
  # 连线末端停在文字锚点外侧一点点（1mm 间隙），不会压到文字上
  expect_true(all(
    grid::convertY(seg$y1[last], "npc", valueOnly = TRUE) > ty
  ))

  # 同一行内按中心距离判断不重叠
  rows <- unique(ty)
  expect_gte(length(rows), 1L)
  for (r in rows) {
    in_row <- which(abs(ty - r) < 1e-9)
    if (length(in_row) < 2L) next
    ord <- order(tx[in_row])
    xs <- tx[in_row][ord]
    ws <- box_along_npc[in_row][ord]
    gap <- diff(xs) * panel - (ws[-length(ws)] + ws[-1L]) / 2
    expect_true(all(gap >= 1e-9), info = sprintf("row y=%f", r))
  }
})

test_that("y 轴标签按列错开，引导线从刻度指向标签", {
  d <- example_data(30)
  p <- ggplot2::ggplot(d, ggplot2::aes(v, g)) + ggplot2::geom_point() +
    ggplot2::scale_y_discrete(guide = guide_axis_smart(nrow = 2))
  lg <- find_label_grob(ggplot2::ggplotGrob(p))

  along <- panel_cm(TRUE)                 # y 轴：沿轴方向是高度
  across <- attr(lg, "ggse_size_cm")      # 横向预留宽度
  expect_equal(lg$vertical, TRUE)
  rendered <- draw_label_grob(lg, across, along)

  seg <- rendered$children[[1L]]
  texts <- rendered$children[-1L]
  tx <- grob_x_npc(texts)
  ty <- grob_y_npc(texts)

  # 引导线起点 x = 1（贴近轴线），y = 原始刻度位置
  first <- seq(1L, by = 3L, length.out = length(texts))
  last <- first + 2L
  expect_true(all(grid::convertX(seg$x0[first], "npc", valueOnly = TRUE) == 1))
  expect_equal(
    unname(grid::convertY(seg$y0[first], "npc", valueOnly = TRUE)),
    lg$pos,
    tolerance = 1e-9
  )
  # 最后一段垂直短横线回到文字中心所在的 y（= 标签中心），并向内收在轴外
  expect_equal(
    unname(grid::convertY(seg$y1[last], "npc", valueOnly = TRUE)),
    unname(ty),
    tolerance = 1e-9
  )
  expect_true(all(grid::convertX(seg$x1[last], "npc", valueOnly = TRUE) < 1))
  # 列方向：文字锚点 x 落在 [0,1]
  expect_true(all(tx >= -1e-9 & tx <= 1 + 1e-9))
  expect_true(all(ty >= -1e-9 & ty <= 1 + 1e-9))
  # nrow = 2 生效：标签分到两列
  expect_equal(length(unique(round(tx, 6))), 2L)
})

# ---- 6. 旋转标签预留高度 ---------------------------------------------------

test_that("angle = 45 的标签按其旋转后的包围盒预留高度，不会被裁切", {
  labels <- paste0("a long label ", seq_len(12))
  pos <- discrete_pos(12)

  plain <- smart_prepare(labels, pos = pos)
  rotated <- smart_prepare(
    labels, pos = pos,
    theme = ggplot2::theme_grey() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1, vjust = 1))
  )

  expect_equal(plain$prep$style$angle, 0)
  expect_equal(rotated$prep$style$angle, 45)

  # 旋转后横向包围盒变高（对长标签一定成立）
  expect_gt(
    max(rotated$prep$box_across_cm[rotated$prep$shown]),
    max(plain$prep$box_across_cm[plain$prep$shown])
  )
  # 轨道总高 >= 单个旋转标签盒高
  expect_gte(rotated$prep$total_across_cm,
             max(rotated$prep$box_across_cm[rotated$prep$shown]))
  # 每一条轨道都能容纳旋转后的标签盒
  expect_gte(rotated$prep$row_h_cm,
             max(rotated$prep$box_across_cm[rotated$prep$shown]))

  # 绘制后标签盒完整地位于预留空间内
  params <- smart_params(angle = 45)
  lg <- .ggse_label_grob(rotated$prep, params)
  across_cm <- rotated$prep$total_across_cm
  rendered <- draw_label_grob(lg, panel_cm(FALSE), across_cm)
  texts <- rendered$children[-1L]
  ty <- grob_y_npc(texts)
  box_across_npc <- lg$box_across / across_cm
  expect_true(all(ty + box_across_npc / 2 <= 1 + 1e-9))
  expect_true(all(ty - box_across_npc / 2 >= -1e-9))

  # 端到端也能画
  d <- data.frame(g = factor(labels), v = seq_along(labels))
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart()) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                       vjust = 1))
  expect_silent(ggplot2::ggplotGrob(p))
})

# ---- 7. sec_axis / facet / 连续轴 -----------------------------------------

test_that("sec.axis + dup_axis(guide = ...) 能渲染", {
  d <- example_data(40)
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(
      guide = guide_axis_smart(n = 10),
      sec.axis = ggplot2::dup_axis(guide = guide_axis_smart(which = c("S1", "S40")))
    )
  expect_silent(gt <- ggplot2::ggplotGrob(p))
  expect_s3_class(gt, "gtable")

  # 主、次轴各有一个智能标签 grob
  count_label_grobs <- function(grob) {
    if (inherits(grob, "ggse_smart_labels")) {
      return(1L)
    }
    kids <- NULL
    if (!is.null(grob$grobs)) kids <- grob$grobs
    if (!is.null(grob$children)) kids <- c(kids, grob$children)
    if (is.null(kids) || length(kids) == 0L) {
      return(0L)
    }
    sum(vapply(kids, count_label_grobs, integer(1L)))
  }
  expect_gte(count_label_grobs(gt), 2L)
})

test_that("facet 与连续轴场景都能画", {
  d <- example_data(30)

  p_facet <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~ v > 0.5) +
    ggplot2::scale_x_discrete(guide = guide_axis_smart(n = 6))
  expect_silent(ggplot2::ggplotGrob(p_facet))

  p_cont <- ggplot2::ggplot(d, ggplot2::aes(v, v)) + ggplot2::geom_point() +
    ggplot2::guides(x = guide_axis_smart(n = 5))
  expect_silent(gt <- ggplot2::ggplotGrob(p_cont))
  lg <- find_label_grob(gt)
  expect_false(is.null(lg))
  expect_length(lg$labels, 5L)
})

# ---- 8. 边界情况 -----------------------------------------------------------

test_that("只有一个标签时也能正常绘制", {
  d <- data.frame(g = factor("only"), v = 1)
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart())
  expect_silent(gt <- ggplot2::ggplotGrob(p))
  lg <- find_label_grob(gt)
  expect_false(is.null(lg))
  expect_identical(lg$labels, "only")
  expect_equal(lg$nrow, 1L)

  prep <- smart_prepare("only", pos = 0.5)$prep
  expect_equal(prep$nrow, 1L)
  expect_true(all(prep$shown))
})

test_that("which 选空时安全返回，不画标签也不报错", {
  d <- example_data(10)
  # 整数(0) / 字符(0) 是"空选择"：什么都不画，也不报错
  for (which in list(integer(0), character(0))) {
    p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
      ggplot2::scale_x_discrete(guide = guide_axis_smart(which = which))
    expect_silent(gt <- ggplot2::ggplotGrob(p))
    expect_null(find_label_grob(gt))
  }
})

test_that("0 行数据 / 无 break 的坐标轴不会报错", {
  d <- data.frame(g = factor(character(0), levels = c("a", "b")), v = numeric(0))
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart())
  expect_silent(ggplot2::ggplotGrob(p))

  prep <- smart_prepare(character(0), pos = numeric(0))
  expect_null(prep$prep)
})

test_that("200 个分类时自动抽稀并由 cli 提示一次，可静音", {
  d <- example_data(200)
  p <- ggplot2::ggplot(d, ggplot2::aes(g, v)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(guide = guide_axis_smart())

  expect_message(
    gt <- ggplot2::ggplotGrob(p),
    "fit into"
  )
  expect_s3_class(gt, "gtable")
  lg <- find_label_grob(gt)
  expect_lt(length(lg$labels), 200L)

  # 静音选项生效
  old <- options(ggutils.quiet = TRUE)
  on.exit(options(old), add = TRUE)
  expect_silent(ggplot2::ggplotGrob(p))
})

# ---- theme inheritance: labels follow axis.text.x / axis.text.y ------------

test_that("smart labels inherit the axis text theme (size, colour, face, family)", {
  labels <- paste0("S", seq_len(6))
  pos <- discrete_pos(6)
  th <- ggplot2::theme_grey() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      size = 20, colour = "red", face = "bold", family = "serif"
    ))
  res <- smart_prepare(labels, pos = pos, theme = th)
  st <- res$prep$style
  expect_equal(st$size, 20)
  expect_identical(st$colour, "red")
  expect_identical(st$face, "bold")
  expect_identical(st$family, "serif")

  yth <- ggplot2::theme_grey() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 14, colour = "blue"))
  res_y <- smart_prepare(labels, pos = pos, aes = "y", position = "left", theme = yth)
  expect_equal(res_y$prep$style$size, 14)
  expect_identical(res_y$prep$style$colour, "blue")
})