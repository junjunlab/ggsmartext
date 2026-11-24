# ggsmartext —— 示例 05：长标签与角度（angle）
#
# 标签很长时，多行错开已经不够用，可以旋转标签：
#   angle = 45：建议 nrow = 1，配合 hjust = 1, vjust = 1（右对齐）
#   angle = 90：建议 hjust = 1, vjust = 0.5
#
# 运行方式（需要先安装 ggsmartext）：
#   Rscript inst/examples/05-long-labels-angle.R

library(ggplot2)
library(ggsmartext)

out_dir <- Sys.getenv("GGSMARTEXT_FIG_DIR", unset = file.path(tempdir(), "ggsmartext-examples"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

save_png <- function(plot, name, width = 1600, height = 900) {
  file <- file.path(out_dir, name)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = width, height = height, res = 150, background = "white")
  } else {
    grDevices::png(file, width = width, height = height, res = 150, bg = "white", type = "cairo")
  }
  print(plot)
  grDevices::dev.off()
  message("written: ", file)
  invisible(file)
}

# ---- 数据：带分组后缀的长中文标签 -------------------------------------------

set.seed(2024)
trt <- rep(c("对照组", "处理组A", "处理组B"), 8)
labs24 <- paste0("样本", 1:24, "（", trt, "）")
df <- data.frame(
  sample = factor(labs24, levels = labs24),
  value = round(runif(24, 10, 100), 1)
)

base_plot <- function(title, guide) {
  ggplot(df, aes(x = sample, y = value)) +
    geom_col(fill = "#5B8FF9", width = 0.8) +
    guide +
    labs(x = NULL, y = "响应值", title = title) +
    theme_grey(base_size = 12)
}

# ---- 1. 长标签 + 多行错开（不旋转） ----------------------------------------

p_rows <- base_plot("长标签多行错开：n = 12, nrow = 2",
                    guides(x = guide_axis_smart(n = 12, nrow = 2)))

# ---- 2. angle = 45 ----------------------------------------------------------

p_45 <- base_plot("angle = 45, hjust = 1, vjust = 1",
                  guides(x = guide_axis_smart(n = 12, nrow = 1, angle = 45,
                                              hjust = 1, vjust = 1)))

# ---- 3. angle = 90 ----------------------------------------------------------

p_90 <- base_plot("angle = 90, hjust = 1, vjust = 0.5",
                  guides(x = guide_axis_smart(n = 12, nrow = 1, angle = 90,
                                              hjust = 1, vjust = 0.5)))

# ---- 输出 -------------------------------------------------------------------

save_png(p_rows, "05-long-rows.png")
save_png(p_45, "05-angle-45.png")
save_png(p_90, "05-angle-90.png")

if (interactive()) print(p_45)
