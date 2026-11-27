#' One-dimensional label-box clustering and packing (a `BoxArrange` port)
#'
#' `box_arrange()` re-implements as pure functions the R5 reference class
#' `BoxArrange` (plus its helpers `Box` and `BoxCluster`) from
#' \pkg{ComplexHeatmap}:
#'
#' 1. sort the boxes by **midpoint** (the input need not be sorted);
#' 2. merge overlapping boxes (touching ends count) into clusters, in midpoint
#'    order;
#' 3. for each cluster use `sum(width)` as its span, lay the boxes end to end
#'    inside `range` (`flatten`), then iteratively merge the overlaps created by
#'    the edge shifts (`merge`);
#' 4. shift clusters that fall outside back into `range` (`adjust_to_range`)
#'    and merge once more;
#' 5. restore the original input order.
#'
#' When the total width exceeds `diff(range)` (the boxes simply do not fit) it
#' falls back to the **proportional spread** of
#' `ComplexHeatmap:::smartAlign2()`: the first and last box stay glued to the
#' ends and the rest are spread proportionally, so overlaps remain but every
#' box stays inside `range`.
#'
#' @param start,end Numeric vectors giving the left and right end of each label
#'   box; they must have the same length. `start` need not be sorted.
#' @param range Numeric vector of length 2, the range the boxes may occupy
#'   (e.g. `c(0, 1)`). Defaults to `range(c(start, end))`.
#'
#' @return A data.frame with columns `start` and `end`, in the **same row order
#'   as the input** (row i is the new position of the i-th input box).
#'
#' @details
#' On feasible input the result matches the reference implementation
#' (`ComplexHeatmap:::smartAlign2()`, the wrapper around `BoxArrange`) element
#' by element within 1e-10. The differences are robustness and reproducibility
#' improvements:
#' * an empty input returns a zero-row data.frame; the reference throws
#'   `subscript out of bounds`;
#' * boxes with identical midpoints are ordered **deterministically** (the
#'   reference uses `rank(ties.method = "random")`, which consumes random
#'   numbers and is not reproducible); results can only differ from the
#'   reference when exact ties occur;
#' * a single box wider than `range` is centred; the reference returns `-Inf`.
#'
#' The reference returns a matrix on the feasible branch and a data.frame on
#' the infeasible one; this function always returns a data.frame.
#'
#' @seealso [smart_align()] for the circlize packing algorithm (which requires
#'   sorted input); [smart_stack()] for the multi-row label engine.
#'
#' @export
#' @examples
#' # two slightly overlapping boxes are packed end to end
#' box_arrange(c(0.10, 0.12), c(0.14, 0.16), c(0, 1))
#'
#' # unsorted input is fine: sorted by midpoint, returned in input order
#' box_arrange(c(0.60, 0.10, 0.30), c(0.64, 0.14, 0.34), c(0, 1))
#'
#' # when the widths exceed the range it falls back to a proportional spread
#' box_arrange(c(0.1, 0.2, 0.3), c(0.6, 0.7, 0.8), c(0, 1))
box_arrange <- function(start, end, range = range(c(start, end))) {
  start <- as.numeric(start)
  end <- as.numeric(end)
  .sa_check_numeric(start, "start")
  .sa_check_numeric(end, "end")
  if (length(start) != length(end)) {
    cli::cli_abort("{.arg start} and {.arg end} must have the same length ({length(start)} vs {length(end)}).")
  }
  .sa_check_range(range)

  n <- length(start)
  if (n == 0L) {
    return(data.frame(start = numeric(0), end = numeric(0)))
  }

  width <- end - start
  if (any(width < 0)) {
    cli::cli_abort("{.arg end} must not be smaller than {.arg start} (negative box width).")
  }

  # 放不下时退回"等比摊开"（= ComplexHeatmap::smartAlign2 的分支）
  if (sum(width) > range[2] - range[1]) {
    return(.ba_spread(start, end, range))
  }

  .ba_arrange(start, end, range)
}

# ---- BoxArrange 数据结构（纯函数化）---------------------------------------

# 盒子：start/end 原始位置，new_start/new_end 排布后的位置
.ba_box <- function(start, end) {
  list(
    start = start, end = end,
    new_start = start, new_end = end,
    height = end - start, mid = (start + end) / 2
  )
}

# cluster：start/end/mid/height 为当前（可能被平移过的）位置，
# boxes 为按中点升序排列的盒子列表
.ba_cluster_new <- function() {
  list(
    start = NA_real_, end = NA_real_, mid = NA_real_, height = NA_real_,
    boxes = list()
  )
}

# 等价于 BoxCluster$add_box()（可一次加入若干盒子）
.ba_cluster_add_boxes <- function(cl, boxes) {
  stopifnot(length(boxes) > 0L)
  cl$boxes <- c(cl$boxes, boxes)
  s <- min(vapply(cl$boxes, `[[`, numeric(1), "start"))
  e <- max(vapply(cl$boxes, `[[`, numeric(1), "end"))
  cl$start <- s
  cl$end <- e
  cl$mid <- (s + e) / 2
  cl$height <- e - s
  cl
}

# 等价于 BoxCluster$update_box_new_pos()
.ba_cluster_update <- function(cl) {
  nb <- length(cl$boxes)
  if (nb == 0L) {
    return(cl)
  }
  h <- vapply(cl$boxes, `[[`, numeric(1), "height")
  new_s <- cl$start + cumsum(c(0, h[-nb]))
  new_e <- cl$start + cumsum(h)
  cl$boxes <- Map(
    function(b, s, e) {
      b$new_start <- s
      b$new_end <- e
      b
    },
    cl$boxes, new_s, new_e
  )
  cl
}

# 等价于 BoxCluster$flatten()
.ba_cluster_flatten <- function(cl, range) {
  s <- min(vapply(cl$boxes, `[[`, numeric(1), "start"))
  e <- max(vapply(cl$boxes, `[[`, numeric(1), "end"))
  cl$mid <- (s + e) / 2
  cl$height <- sum(vapply(cl$boxes, `[[`, numeric(1), "height"))
  s2 <- cl$mid - cl$height / 2
  e2 <- cl$mid + cl$height / 2
  if (s2 < range[1]) {
    s2 <- range[1]
    e2 <- s2 + cl$height
  } else if (e2 > range[2]) {
    e2 <- range[2]
    s2 <- e2 - cl$height
  }
  cl$start <- s2
  cl$end <- e2
  .ba_cluster_update(cl)
}

# 等价于 BoxCluster$move()
.ba_cluster_move <- function(cl, x) {
  cl$start <- cl$start + x
  cl$end <- cl$end + x
  cl$mid <- cl$mid + x
  .ba_cluster_update(cl)
}

# 等价于 ComplexHeatmap:::box_overlap()（首尾相接也算重叠）
.ba_overlap <- function(start1, end1, start2, end2) {
  if (end1 < start2) {
    FALSE
  } else if (end2 < start1) {
    FALSE
  } else {
    TRUE
  }
}

# 等价于 BoxArrange$merge()
# 注意：这里需要一个"已合并"哨兵但**不能删除元素**（删除会让下标前移，
# 与参考实现 `cl[[i-1]] = NA` 的语义不同），所以用长度 1 的 NA 占位。
.ba_merge <- function(cl, range) {
  repeat {
    merged <- FALSE
    n_cluster <- length(cl)
    for (i in seq_len(n_cluster)[-1]) {
      if (.ba_overlap(cl[[i]]$start, cl[[i]]$end, cl[[i - 1]]$start, cl[[i - 1]]$end)) {
        cl[[i - 1]] <- .ba_cluster_add_boxes(cl[[i - 1]], cl[[i]]$boxes)
        cl[[i]] <- cl[[i - 1]]
        cl[[i - 1]] <- NA
        merged <- TRUE
      }
    }
    if (!merged) {
      break
    }
    cl <- cl[!vapply(cl, .ba_is_merged_sentinel, logical(1))]
    cl <- lapply(cl, .ba_cluster_flatten, range = range)
  }
  cl
}

.ba_is_merged_sentinel <- function(x) {
  !is.list(x) && length(x) == 1L && is.na(x)
}

# 等价于 BoxArrange$adjust_to_range()（末尾自带一次 merge）
.ba_adjust_to_range <- function(cl, range) {
  cl <- lapply(cl, function(clu) {
    if (clu$start < range[1]) {
      clu <- .ba_cluster_move(clu, range[1] - clu$start)
    }
    if (clu$end > range[2]) {
      clu <- .ba_cluster_move(clu, range[2] - clu$end)
    }
    clu
  })
  .ba_merge(cl, range)
}

# 等价于 BoxArrange$initialize() 的分析阶段（不含 plot / merge / adjust）
.ba_arrange <- function(start, end, range) {
  n <- length(start)
  boxes <- lapply(seq_len(n), function(i) .ba_box(start[i], end[i]))
  mid <- vapply(boxes, `[[`, numeric(1), "mid")
  ord <- order(mid)
  boxes <- boxes[ord]
  # 中点 -> 升序名次（确定性平局；参考实现是 ties.method = "random"）
  rank_in_sorted <- rank(mid, ties.method = "first")

  # 依中点次序构造重叠 cluster
  cl <- list(.ba_cluster_add_boxes(.ba_cluster_new(), boxes[1]))
  i_cluster <- 1L
  for (i in seq_len(n)[-1]) {
    if (.ba_overlap(cl[[i_cluster]]$start, cl[[i_cluster]]$end, boxes[[i]]$start, boxes[[i]]$end)) {
      cl[[i_cluster]] <- .ba_cluster_add_boxes(cl[[i_cluster]], boxes[i])
    } else {
      i_cluster <- i_cluster + 1L
      cl[[i_cluster]] <- .ba_cluster_add_boxes(.ba_cluster_new(), boxes[i])
    }
  }

  # 与 BoxArrange$initialize() 相同的调用序列
  cl <- lapply(cl, .ba_cluster_flatten, range = range)
  cl <- .ba_merge(cl, range)
  cl <- .ba_adjust_to_range(cl, range)
  cl <- .ba_merge(cl, range)

  pos <- do.call(
    rbind,
    lapply(cl, function(clu) {
      do.call(rbind, lapply(clu$boxes, function(b) c(b$new_start, b$new_end)))
    })
  )
  pos <- pos[rank_in_sorted, , drop = FALSE]
  data.frame(start = pos[, 1], end = pos[, 2])
}

# 放不下时的等比摊开：与 ComplexHeatmap:::smartAlign2() 的可选分支一致
.ba_spread <- function(start, end, range) {
  n <- length(start)
  h_raw <- end - start
  if (n == 1L) {
    # 参考实现在此会得到 -Inf（mid_radius = 0）；这里改为居中
    center <- (range[1] + range[2]) / 2
    return(data.frame(
      start = center - h_raw / 2,
      end = center + h_raw / 2
    ))
  }
  mid <- (start + end) / 2
  od <- order(mid)
  h <- h_raw[od]
  mid_diff <- numeric(n)
  mid_diff[1] <- 0
  mid_diff[2:n] <- h[2:n] / 2 + h[1:(n - 1)] / 2
  mid_radius <- sum(h) - h[n] / 2 - h[1] / 2
  a_1 <- range[1] + h[1] / 2
  a_n <- range[2] - h[n] / 2
  a <- a_1 + cumsum(mid_diff) / mid_radius * (a_n - a_1)
  new_start <- numeric(n)
  new_end <- numeric(n)
  new_start[od] <- a - h / 2
  new_end[od] <- a + h / 2
  data.frame(start = new_start, end = new_end)
}
