#' 一维标签盒防重叠压紧算法（smartAlign 移植）
#'
#' `smart_align()` 是 \pkg{circlize} 包中 `smartAlign()` 算法的精确移植：
#' 给定一组按位置升序排列、允许相互重叠的一维区间（标签盒），
#' 通过**迭代聚类 + 压紧**的方式消除重叠：相互重叠的盒子被并成一个
#' cluster，cluster 内按原有宽度首尾相接重新排布，并把 cluster 整体
#' 平移到其原始中点附近；只要不越界就尽量少移动。迭代直到 cluster
#' 的数目不再变化为止。
#'
#' 该函数是纯函数（不修改环境、不使用随机数），结果可重复。
#'
#' @param start,end 数值向量，标签盒的左右端点（任意一致的单位，
#'   例如 npc 或数据坐标）。两个向量长度必须相同。`start` 必须按
#'   升序排列（这是算法假设，与参考实现一致；乱序输入请先
#'   `order()`，或改用 [box_arrange()]——它会自行按中点排序）。
#' @param range 长度为 2 的数值向量，允许排布的范围（如 `c(0, 1)`）。
#'   默认 `range(c(start, end))`。
#'
#' @return 一个 data.frame，含 `start`、`end` 两列，**行顺序与输入完全
#'   一致**（第 i 行是输入第 i 个盒子的新位置）。
#'
#' @details
#' 与参考实现的差异只有两处，均为健壮性改进：
#' * 空输入返回 0 行的 data.frame（`circlize::smartAlign()` 会报错）；
#' * 输入不合法（长度不一致、含非有限值、`range` 非法）时给出清晰的报错。
#'
#' 注意：若所有盒子宽度之和大于 `diff(range)`，算法无法消除重叠，
#' 此时（与参考实现一致）会把该 cluster 居中放置，结果会**超出**
#' `range`。调用方应在调用前自己判断可行性（[smart_stack()] 会先丢弃
#' 低优先级标签再调用本函数）。
#'
#' @seealso [box_arrange()]：另一种（`ComplexHeatmap` 的 BoxArrange）
#'   排布算法，允许输入乱序；[smart_stack()]：多行标签排布引擎。
#'
#' @export
#' @examples
#' # 两个盒子轻微重叠：被压成首尾相接的一对
#' smart_align(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
#'
#' # 四个完全重叠的盒子：在原始中点处摊开
#' smart_align(rep(0.5, 4), rep(0.6, 4), c(0, 1))
#'
#' # 不重叠的输入原样返回
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
