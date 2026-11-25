# ggutils —— 示例 01：最基础的 x 轴用法
#
# 50 个分类水平时，ggplot2 默认会把每一个标签都画出来，互相压在一起。
# guide_axis_smart() 只显示其中一部分标签，并把它们分成多行错开。
#
# 运行方式（需要先安装 ggutils）：
#   Rscript inst/examples/01-basic-x.R
#
# 图片默认写入 tempdir()；设置环境变量 ggutils_FIG_DIR 可指定输出目录：
#   Sys.setenv(ggutils_FIG_DIR = "figures"); source("inst/examples/01-basic-x.R")

library(ggplot2)
library(ggutils)

out_dir <- Sys.getenv("ggutils_FIG_DIR", unset = file.path(tempdir(), "ggutils-examples"))
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

# ---- 1. 默认的 ggplot2 轴：50 个标签互相重叠 --------------------------------

p_default <- ggplot(df, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  labs(x = NULL, y = "响应值", title = "默认轴：50 个标签挤成一团") +
  theme_grey(base_size = 12)

# ---- 2. 只显示 10 个标签，分 2 行错开 --------------------------------------

p_smart <- ggplot(df, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(n = 10, nrow = 2)) +
  labs(x = NULL, y = "响应值", title = "guide_axis_smart(n = 10, nrow = 2)") +
  theme_grey(base_size = 12)

# ---- 3. 只显示用户指定的几个标签 -------------------------------------------

p_which <- ggplot(df, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(which = c(1, 10, 20, 30, 40, 50), nrow = 2)) +
  labs(x = NULL, y = "响应值", title = "which = c(1, 10, 20, 30, 40, 50)") +
  theme_grey(base_size = 12)

# ---- 输出 -------------------------------------------------------------------

save_png(p_default, "01-default.png")
save_png(p_smart, "01-basic.png")
save_png(p_which, "01-which.png")

# 在交互式会话里直接打印对象即可看到效果（脚本方式跑时只在交互式下打印）
if (interactive()) print(p_smart)
