# ggsmartext —— 示例 04：y 轴（水平分类轴）用法
#
# y 轴是竖直方向，所以 nrow 的语义变成"列数"：标签在轴外侧分成 nrow 列错开。
# 40 个样本的水平条形图，只显示其中 10 个标签，分 2 列。
#
# 运行方式（需要先安装 ggsmartext）：
#   Rscript inst/examples/04-y-axis.R

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
# 条形图里 y 的因子水平从下往上排，反转 levels 让"样本1"在最上面
labs40 <- paste0("样本", 1:40)
df <- data.frame(
  sample = factor(labs40, levels = rev(labs40)),
  value = round(runif(40, 10, 100), 1)
)

base_plot <- function(title, guide) {
  ggplot(df, aes(x = value, y = sample)) +
    geom_col(fill = "#F6903D", width = 0.8) +
    guide +
    labs(x = "响应值", y = NULL, title = title) +
    theme_grey(base_size = 12)
}

# ---- 1. 默认轴：40 个标签太密 ----------------------------------------------

p_default <- base_plot("默认 y 轴：标签挤在一起", NULL)

# ---- 2. 只显示 10 个标签，分 2 列 ------------------------------------------

p_smart <- base_plot("guide_axis_smart(n = 10, nrow = 2)：2 列错开",
                     guides(y = guide_axis_smart(n = 10, nrow = 2)))

# ---- 3. 3 列 + 指定子集 -----------------------------------------------------

p_cols <- base_plot("which = seq(1, 40, by = 4), nrow = 3",
                    guides(y = guide_axis_smart(which = seq(1, 40, by = 4), nrow = 3)))

# ---- 输出 -------------------------------------------------------------------

save_png(p_default, "04-y-default.png")
save_png(p_smart, "04-y-smart.png")
save_png(p_cols, "04-y-columns.png")

if (interactive()) print(p_smart)
