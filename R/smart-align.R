#' One-dimensional label-box de-overlap packing (a `smartAlign()` port)
#'
#' `smart_align()` is an exact port of `smartAlign()` from \pkg{circlize}:
#' given one-dimensional intervals (label boxes) sorted by position and allowed
#' to overlap, it removes the overlaps by **iterative clustering and packing**.
#' Overlapping boxes are merged into a cluster, the cluster is re-laid out
#' inside its own span with the boxes placed end to end at their original
#' widths, and the cluster is then shifted back towards its original midpoint,
#' moving as little as possible while staying in range. Iteration stops when
#' the number of clusters no longer changes.
#'
#' The function is pure (it touches no environment and uses no random
#' numbers), so results are reproducible.
#'
#' @param start,end Numeric vectors giving the left and right end of each label
#'   box, in any consistent unit (npc, data units, ...). The two vectors must
#'   have the same length. `start` must be sorted increasingly (an assumption
#'   of the algorithm, matching the reference implementation); for unsorted
#'   input call `order()` first, or use [box_arrange()], which sorts by
#'   midpoint itself.
#' @param range Numeric vector of length 2, the range the boxes may occupy
#'   (e.g. `c(0, 1)`). Defaults to `range(c(start, end))`.
#'
#' @return A data.frame with columns `start` and `end`, in the **same row order
#'   as the input** (row i is the new position of the i-th input box).
#'
#' @details
#' There are only two differences from the reference implementation, both of
#' them robustness improvements:
#' * an empty input returns a zero-row data.frame (`circlize::smartAlign()`
#'   throws an error);
#' * invalid input (unequal lengths, non-finite values, a bad `range`) raises a
#'   clear error.
#'
#' Note that when the total width of the boxes exceeds `diff(range)` the
#' overlaps cannot be removed: the cluster is centred (as in the reference
#' implementation) and the result **sticks out** of `range`. Callers should
#' check feasibility first ([smart_stack()] drops low-priority labels before
#' calling this function).
#'
#' @seealso [box_arrange()] for the other packing algorithm (BoxArrange from
#'   `ComplexHeatmap`), which also accepts unsorted input; [smart_stack()] for
#'   the multi-row label engine.
#'
#' @export
#' @examples
#' # two slightly overlapping boxes: packed end to end
#' smart_align(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
#'
#' # four identical boxes: spread around the original midpoint
#' smart_align(rep(0.5, 4), rep(0.6, 4), c(0, 1))
#'
#' # non-overlapping input is returned unchanged
#' smart_align(c(0.1, 0.5), c(0.2, 0.6), c(0, 1))
smart_align <- function(start, end, range = range(c(start, end))) {
  start <- as.numeric(start)
  end <- as.numeric(end)
  .sa_check_numeric(start, "start")
  .sa_check_numeric(end, "end")
  if (length(start) != length(end)) {
    cli::cli_abort("{.arg start} and {.arg end} must have the same length ({length(start)} vs {length(end)}).")
  }
  .sa_check_range(range)

  if (length(start) == 0L) {
    return(data.frame(start = numeric(0), end = numeric(0)))
  }

  .sal_core(start, end, range)
}

# 算法内核：x1/x2 必须按"沿轴依次排列"的次序给出（即相邻元素沿轴向右，
# 对 smart_stack 来说就是按中心位置排序）。`smart_align()` 直接原样传入，
# 因此与 circlize::smartAlign 逐元素一致；`smart_stack()` 传入的是按中心
# 排序的区间，算法对"中心有序"的输入同样成立。
#
# 返回 data.frame(start, end)，行序与输入一致。
.sal_core <- function(x1, x2, range) {
  ncluster_before <- -1
  ncluster <- length(x1)

  # 迭代：cluster 数不再变化时收敛（与 circlize::smartAlign 完全一致）
  while (ncluster_before != ncluster) {
    ncluster_before <- ncluster

    cluster <- rep(0, length(x1))
    i_cluster <- 1
    cluster[1] <- i_cluster
    for (i in seq_along(x1)[-1]) {
      if (x1[i] <= x2[i - 1]) {
        cluster[i] <- i_cluster
      } else {
        i_cluster <- i_cluster + 1
        cluster[i] <- i_cluster
      }
    }
    ncluster <- length(unique(cluster))
    if (ncluster_before == ncluster) {
      break
    }

    new_x1 <- numeric(length(x1))
    new_x2 <- numeric(length(x2))
    for (ic in unique(cluster)) {
      index <- which(cluster == ic)
      len <- x2[index] - x1[index]
      total_len <- sum(len)
      mid <- (min(x1[index]) + max(x2[index])) / 2
      if (total_len > range[2] - range[1]) {
        tp <- c(0, cumsum(len)) + (range[1] + range[2]) / 2 - total_len / 2
      } else if (mid - total_len / 2 < range[1]) {
        tp <- c(0, cumsum(len)) + range[1]
      } else if (mid + total_len / 2 > range[2]) {
        tp <- range[2] - total_len + c(0, cumsum(len))
      } else {
        tp <- c(0, cumsum(len)) + mid - total_len / 2
      }
      new_x1[index] <- tp[-length(tp)]
      new_x2[index] <- tp[-1]
    }
    x1 <- new_x1
    x2 <- new_x2
  }

  data.frame(start = x1, end = x2)
}

# ---- internal helpers ------------------------------------------------------

.sa_check_numeric <- function(x, arg) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector.")
  }
  if (length(x) && any(!is.finite(x))) {
    cli::cli_abort("{.arg {arg}} must not contain {.val NA}, {.val Inf} or {.val NaN}.")
  }
  invisible(x)
}

.sa_check_range <- function(range) {
  if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range))) {
    cli::cli_abort("{.arg range} must be a finite numeric vector of length 2, e.g. {.code c(0, 1)}.")
  }
  if (range[1] > range[2]) {
    cli::cli_abort("{.arg range} must be an increasing interval ({range[1]} > {range[2]}).")
  }
  invisible(range)
}
