#' 多行标签排布引擎
#'
#' `smart_stack()` 把一系列标签盒（每个盒子有轴线上的中心位置 `positions`
#' 与宽度 `widths`，单位均为 npc）分配到 `n` 行（y 轴时为 `n` 列）中的某
#' 一行，使每个标签都能画出来、行内不重叠、并且尽量贴近原始位置。它是
#' `guide_axis_smart()` 的核心排布算法，也可以单独使用。
#'
#' 算法分三步：
#'
#' 1. **可用范围**：可用范围 `range` 按 `extend` 向两侧各扩展
#'    `extend * diff(range)`，得到 `lim`；宽度大于 `diff(lim)` 的标签盒
#'    无论如何放不下，直接丢弃。
#' 2. **分配行**：按优先级从高到低（`priority` 越大越优先；平局按位置、
#'    再按下标）逐个分配。对每个标签盒，在所有"还有容量"的行里选择当前
#'    占用最少的一行（占用 = 行内宽度之和 + 必要的 `padding` 间隙），因此
#'    当所有标签优先级相同时，结果会沿轴交替落入各行（水填式均衡）。
#'    所有行都放不下的标签被丢弃（`shown = FALSE`）。
#' 3. **行内压紧**：每行内按标签盒中心位置保持轴序，把盒子左右各按
#'    `padding / 2` 膨胀后交给压紧算法消除重叠，再收缩回来，于是同一行内
#'    相邻标签之间恰好留出 `padding` 的间隙。膨胀前会用保序投影把中心夹进
#'    可用范围，因此结果绝不会越出 `lim`。
#'
#' @param positions 数值向量，每个标签盒在轴上的中心位置（npc，0..1）。
#'   不要求有序。
#' @param widths 数值向量，每个标签盒的宽度（npc），与 `positions` 等长。
#' @param n 正整数，行数（x 轴为行、y 轴为列）。也决定"总容量"上限。
#' @param range 长度为 2 的数值向量，轴的可绘制范围，默认 `c(0, 1)`。
#' @param extend 非负标量：允许标签超出 `range` 的比例（`0.02` 表示两侧各
#'   放宽轴跨度的 0.02 倍），与 `guide_axis_smart()` 的同名参数一致。
#' @param padding 非负标量：同一行内相邻标签盒之间要求的最小间隙，
#'   单位与 `positions` 相同（npc）。
#' @param priority `NULL`（默认，所有标签同等重要）或与 `positions` 等长
#'   的数值向量。**数值越大越优先保留**；放不下时优先丢弃低优先级标签。
#'
#' @return 一个 data.frame，行数与输入等长、顺序与输入一致，列为：
#' * `index`：输入下标（`1..length(positions)`）；
#' * `row`：分配到的行号（`1..n`）；被丢弃的标签为 `NA`；
#' * `start`, `end`：排布后标签盒的左右端点（npc）；
#' * `center`：排布后标签盒中心（npc）；
#' * `shown`：逻辑值，该标签是否应当绘制。
#'
#' @details
#' * 所有 `shown = TRUE` 的盒子都满足：`lim[1] <= start < end <= lim[2]`；
#'   同一行内按 `start` 排序后满足 `start[i+1] >= end[i] + padding`（在没有
#'   触发贴边剪裁时取等号）。
#' * 行内顺序按标签盒**中心位置**保持轴序，与输入位置顺序一致。
#' * 行的分配由优先级 + 负载均衡决定，是**确定性**的；同一输入必然得到
#'   同一输出，不消耗随机数。
#' * 函数保证"能画就不重叠"，但不会为了塞下更多标签而把标签压得比
#'   `widths` 更窄——放不下就丢弃（`shown = FALSE`）。
#' * 当标签很多、`n` 行总容量不足时，被丢弃的是优先级最低（`priority`
#'   最小）的那些；优先级相同时，先处理靠轴左侧的标签。
#'
#' @seealso [smart_align()], [box_arrange()]
#'
#' @export
#' @examples
#' # 6 个标签均匀分布，分 2 行：交替落入两行，几乎不需要移动
#' pos <- seq(0.1, 0.9, length.out = 6)
#' smart_stack(pos, widths = rep(0.08, 6), n = 2)
#'
#' # 3 个标签挤在 0.5 附近，分 3 行也不会重叠
#' smart_stack(c(0.48, 0.5, 0.52), widths = rep(0.2, 3), n = 3)
#'
#' # 容量不足时按 priority 丢弃：高优先级保留
#' smart_stack(
#'   positions = rep(0.5, 5), widths = rep(0.4, 5), n = 1,
#'   priority = c(1, 5, 3, 4, 2)
#' )
smart_stack <- function(positions, widths, n,
                        range = c(0, 1), extend = 0, padding = 0,
                        priority = NULL) {
  positions <- as.numeric(positions)
  widths <- as.numeric(widths)
  .sa_check_numeric(positions, "positions")
  .sa_check_numeric(widths, "widths")
  if (length(positions) != length(widths)) {
    cli::cli_abort("{.arg positions} and {.arg widths} must have the same length ({length(positions)} vs {length(widths)}).")
  }
  if (any(widths < 0)) {
    cli::cli_abort("{.arg widths} must not contain negative values.")
  }
  .sa_check_range(range)

  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 1 || n != round(n)) {
    cli::cli_abort("{.arg n} must be a single integer >= 1.")
  }
  n <- as.integer(n)

  extend <- .ss_check_scalar(extend, "extend")
  padding <- .ss_check_scalar(padding, "padding")

  m <- length(positions)
  if (m == 0L) {
    return(data.frame(
      index = integer(0), row = integer(0),
      start = numeric(0), end = numeric(0), center = numeric(0),
      shown = logical(0)
    ))
  }

  if (is.null(priority)) {
    prio <- rep(0, m)
  } else {
    prio <- as.numeric(priority)
    .sa_check_numeric(prio, "priority")
    if (length(prio) != m) {
      cli::cli_abort("{.arg priority} must have the same length as {.arg positions} ({length(prio)} vs {m}).")
    }
  }

  span <- range[2] - range[1]
  lim <- range + c(-1, 1) * extend * span
  span_lim <- lim[2] - lim[1]
  gap <- padding
  tol <- span_lim * 1e-12

  start <- positions - widths / 2
  end <- positions + widths / 2
  # 宽度超过整行可用宽度的标签盒放不下
  too_wide <- widths > span_lim + tol

  # ---- 1. 分配行（优先级降序 + 负载均衡）---------------------------------
  row_of <- rep(NA_integer_, m)
  load_w <- numeric(n) # 每行已占宽度
  cnt <- integer(n) # 每行已放标签数
  ord <- order(-prio, positions, seq_len(m))
  for (k in ord) {
    if (too_wide[k]) {
      next
    }
    # 放进第 r 行后，该行需要 sum(width) + 盒子数 * gap 的最终空间
    fits <- which(load_w + widths[k] + (cnt + 1) * gap <= span_lim + tol)
    if (!length(fits)) {
      next
    }
    r <- fits[which.min(load_w[fits] + cnt[fits] * gap)]
    row_of[k] <- r
    load_w[r] <- load_w[r] + widths[k]
    cnt[r] <- cnt[r] + 1L
  }

  # ---- 2. 行内压紧 --------------------------------------------------------
  for (r in seq_len(n)) {
    idx <- which(row_of == r)
    if (!length(idx)) {
      next
    }
    # 行内按中心位置（= 轴序）排列
    idx <- idx[order(positions[idx], idx)]
    w <- widths[idx]
    p <- positions[idx]
    half <- (w + gap) / 2
    lo <- lim[1] + half
    hi <- lim[2] - half
    # 保序地把中心夹进 lim：lower 是前缀下界的累计最大、upper 是后缀上界的
    # 累计最小，二者都单调不减；行容量保证 max(w + gap) <= span_lim，故
    # lower <= upper 处处成立，该投影一定存在。
    lower <- cummax(lo)
    upper <- rev(cummin(rev(hi)))
    if (all(lower <= upper + tol)) {
      p <- pmin(pmax(p, lower), upper)
    } else {
      p <- pmin(pmax(p, lo), hi) # 理论上不会发生（兜底）
    }
    aligned <- .sal_core(p - half, p + half, lim)
    start[idx] <- aligned$start + gap / 2
    end[idx] <- aligned$end - gap / 2
  }

  out <- data.frame(
    index = seq_len(m),
    row = row_of,
    start = start,
    end = end,
    center = (start + end) / 2,
    shown = !is.na(row_of)
  )
  out
}

# ---- internal helpers ------------------------------------------------------

.ss_check_scalar <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    cli::cli_abort("{.arg {arg}} must be a single finite number >= 0.")
  }
  as.numeric(x)
}
