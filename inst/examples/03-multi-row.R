# ggsmartext —— 示例 03：多行错开、行距与引导线样式
#
# nrow 控制 x 轴标签占几行；gap / padding 控制行距和标签盒间距；
# connection_style 有 "diagonal"（默认，短垂线 + 斜线 + 短垂线，circlize /
# ComplexHeatmap::anno_mark 的经典几何）、"straight"（一条斜线）和
# "elbow"（垂直段 + 平行于轴的台阶）三种。
#
# 运行方式（需要先安装 ggsmartext）：
#   Rscript inst/examples/03-multi-row.R

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

# ---- 数据 -------------------------------------------------------------------

set.seed(2024)
df <- data.frame(
  sample = factor(paste0("样本", 1:50), levels = paste0("样本", 1:50)),
  value = round(runif(50, 10, 100), 1)
)

base_plot <- function(title, guide) {
  ggplot(df, aes(x = sample, y = value)) +
    geom_col(fill = "#5B8FF9", width = 0.8) +
    guide +
    labs(x = NULL, y = "响应值", title = title) +
    theme_minimal(base_size = 12)
}

# ---- 1. 一行 vs 两行 vs 三行 ------------------------------------------------

p1 <- base_plot("n = 8, nrow = 1（一行，标签必须挤得下）",
                guides(x = guide_axis_smart(n = 8, nrow = 1)))
p2 <- base_plot("n = 16, nrow = 2（两行错开）",
                guides(x = guide_axis_smart(n = 16, nrow = 2)))
p3 <- base_plot("n = 24, nrow = 3（三行，充分利用纵向空间）",
                guides(x = guide_axis_smart(n = 24, nrow = 3)))

# ---- 2. nrow = NULL：自动决定行数 ------------------------------------------

p_auto <- base_plot("n = 30, nrow = NULL（自动分行）",
                    guides(x = guide_axis_smart(n = 30)))

# ---- 3. 引导线样式：diagonal（默认）vs straight vs elbow --------------------

p_diagonal <- base_plot('connection_style = "diagonal"（默认）',
                        guides(x = guide_axis_smart(n = 12, nrow = 2,
                                                    connection_style = "diagonal")))
p_straight <- base_plot('connection_style = "straight"',
                        guides(x = guide_axis_smart(n = 12, nrow = 2,
                                                    connection_style = "straight")))
p_elbow <- base_plot('connection_style = "elbow"',
                     guides(x = guide_axis_smart(n = 12, nrow = 2,
                                                 connection_style = "elbow")))

# ---- 4. 引导线长度、行距与线样式微调 ---------------------------------------

p_tuned <- base_plot("link_length = 5, gap = 0.5, connection_col = 'grey40'",
                     guides(x = guide_axis_smart(
                       n = 16, nrow = 2,
                       gap = 0.5, padding = 0.5,
                       link_length = 5,
                       connection_col = "grey40",
                       connection_lwd = 0.5,
                       connection_lty = 2
                     )))

# ---- 输出 -------------------------------------------------------------------

save_png(p1, "03-nrow-1.png")
save_png(p2, "03-nrow-2.png")
save_png(p3, "03-nrow-3.png")
save_png(p_auto, "03-nrow-auto.png")
save_png(p_diagonal, "03-diagonal.png")
save_png(p_straight, "03-straight.png")
save_png(p_elbow, "03-elbow.png")
save_png(p_tuned, "03-tuned.png")

if (interactive()) print(p3)
