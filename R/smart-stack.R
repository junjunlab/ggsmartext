#' Multi-row label placement engine
#'
#' `smart_stack()` assigns a set of label boxes (each with an axis position
#' `positions` and a width `widths`, both in npc) to one of `n` rows (`n`
#' columns for a y axis) so that every label can be drawn, labels within a row
#' do not overlap, and each label stays as close as possible to its original
#' position. It is the placement engine behind `guide_axis_smart()` and can also
#' be used on its own.
#'
#' The algorithm has three steps:
#'
#' 1. **Usable range**: `range` is widened on both sides by `extend *
#'    diff(range)` to give `lim`; boxes wider than `diff(lim)` can never fit
#'    and are dropped.
#' 2. **Row assignment**: boxes are processed from the highest to the lowest
#'    priority (larger `priority` first; ties by position, then by index). Each
#'    box goes to the row that currently carries the least load among the rows
#'    that still have capacity (load = sum of widths in the row plus the
#'    `padding` gaps it needs), so with equal priorities the labels alternate
#'    between rows along the axis (water-filling balance). Labels that fit in no
#'    row are dropped (`shown = FALSE`).
#' 3. **Packing inside a row**: within a row the boxes keep their axis order by
#'    centre position; each box is inflated by `padding / 2` on both sides, the
#'    packing algorithm removes the overlaps, and the boxes are shrunk back, so
#'    neighbouring labels in a row end up exactly `padding` apart. An
#'    order-preserving projection clamps the centres into the usable range
#'    before inflation, so the result never leaves `lim`.
#'
#' @param positions Numeric vector of axis positions (npc, 0..1) of the label
#'   boxes. Does not need to be sorted.
#' @param widths Numeric vector of box widths (npc), the same length as
#'   `positions`.
#' @param n Positive integer, the number of rows (columns for a y axis). It
#'   also sets the total capacity.
#' @param range Numeric vector of length 2, the drawable range of the axis,
#'   default `c(0, 1)`.
#' @param extend Non-negative scalar, how far labels may stick out of `range`
#'   (`0.02` widens the axis span by 0.02 on each side); same meaning as the
#'   argument of the same name in `guide_axis_smart()`.
#' @param padding Non-negative scalar, the minimum gap required between
#'   neighbouring boxes in the same row, in the same unit as `positions` (npc).
#' @param priority `NULL` (default, all labels equally important) or a numeric
#'   vector the same length as `positions`. **Larger values are kept first**;
#'   when space runs out the low-priority labels are dropped.
#'
#' @return A data.frame with one row per input box, in input order, with
#'   columns:
#' * `index`: the input index (`1..length(positions)`);
#' * `row`: the assigned row (`1..n`); `NA` for dropped labels;
#' * `start`, `end`: the left and right end of the placed box (npc);
#' * `center`: the centre of the placed box (npc);
#' * `shown`: logical, whether the label should be drawn.
#'
#' @details
#' * Every box with `shown = TRUE` satisfies
#'   `lim[1] <= start < end <= lim[2]`; within a row, after sorting by `start`,
#'   `start[i+1] >= end[i] + padding` (equality holds when no edge clamping
#'   kicks in).
#' * The order inside a row follows the box **centres**, i.e. the axis order.
#' * Row assignment depends on priority and load balancing only and is
#'   **deterministic**: the same input always gives the same output, with no
#'   random numbers involved.
#' * The function guarantees "drawn means not overlapping", but it never
#'   squeezes a label below its `widths`; labels that do not fit are dropped
#'   (`shown = FALSE`).
#' * When there are many labels and the `n` rows do not provide enough
#'   capacity, the lowest-priority labels are dropped first; among equal
#'   priorities the labels nearest the left end of the axis are processed
#'   first.
#'
#' @seealso [smart_align()], [box_arrange()]
#'
#' @export
#' @examples
#' # six labels spread along the axis in 2 rows: alternate, barely moved
#' pos <- seq(0.1, 0.9, length.out = 6)
#' smart_stack(pos, widths = rep(0.08, 6), n = 2)
#'
#' # three labels crowded around 0.5 still do not overlap in 3 rows
#' smart_stack(c(0.48, 0.5, 0.52), widths = rep(0.2, 3), n = 3)
#'
#' # when space runs out, priority decides what is dropped
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
