# ggutils —— 示例 02：用 which 指定要显示哪些标签
#
# which 支持四种写法：
#   整数向量 —— 按 breaks 顺序的下标
#   字符向量 —— 按标签文本匹配
#   逻辑向量 —— 与标签一一对应，TRUE 显示
#   函数     —— function(labels, positions) 返回逻辑向量，positions 是 0..1 的轴内相对坐标
#
# 运行方式（需要先安装 ggutils）：
#   Rscript inst/examples/02-which-subset.R

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

base_plot <- function(title) {
  ggplot(df, aes(x = sample, y = value)) +
    geom_col(fill = "#5B8FF9", width = 0.8) +
    labs(x = NULL, y = "响应值", title = title) +
    theme_grey(base_size = 12)
}

# ---- 1. 整数下标：每 10 个显示一个 -----------------------------------------

p_int <- base_plot("which = seq(1, 50, by = 10)") +
  guides(x = guide_axis_smart(which = seq(1, 50, by = 10), nrow = 2))

# ---- 2. 字符向量：按标签文本挑 ---------------------------------------------

p_chr <- base_plot('which = c("样本1", "样本2", "样本25", "样本50")') +
  guides(x = guide_axis_smart(which = c("样本1", "样本2", "样本25", "样本50"), nrow = 1))

# ---- 3. 逻辑向量：TRUE 的标签显示 ------------------------------------------

p_lgl <- base_plot('which = grepl("0$", levels(sample))') +
  guides(x = guide_axis_smart(which = grepl("0$", levels(df$sample)), nrow = 1))

# ---- 4. 函数：按位置挑两端 15% 的标签 --------------------------------------

p_fun <- base_plot("which = function(labels, positions) positions < 0.15 | positions > 0.85") +
  guides(x = guide_axis_smart(
    which = function(labels, positions) positions < 0.15 | positions > 0.85,
    nrow = 2
  ))

# ---- 输出 -------------------------------------------------------------------

save_png(p_int, "02-which-integer.png")
save_png(p_chr, "02-which-character.png")
save_png(p_lgl, "02-which-logical.png")
save_png(p_fun, "02-which-function.png")

if (interactive()) print(p_int)
