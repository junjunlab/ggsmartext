# ggutils —— 示例 06：连续轴也能用
#
# 连续轴的刻度也可以通过 scale_*_continuous(breaks = ...) 设得很密，
# 此时 guide_axis_smart() 同样可以只显示其中一部分、并分多行错开。
#
# 运行方式（需要先安装 ggutils）：
#   Rscript inst/examples/06-continuous.R

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

# ---- 数据：一条噪声曲线 -----------------------------------------------------

set.seed(2024)
df <- data.frame(
  x = seq(0, 100, length.out = 200),
  y = sin(seq(0, 100, length.out = 200) / 12) + rnorm(200, sd = 0.15)
)

# ---- 1. 51 个刻度的连续轴：默认挤成一团 ------------------------------------

p_default <- ggplot(df, aes(x, y)) +
  geom_line(colour = "#5B8FF9", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 100, by = 2), minor_breaks = NULL) +
  labs(x = NULL, y = "响应值", title = "默认轴：51 个刻度标签") +
  theme_grey(base_size = 12)

# ---- 2. 只显示 10 个刻度标签 -------------------------------------------------

p_smart <- ggplot(df, aes(x, y)) +
  geom_line(colour = "#5B8FF9", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 100, by = 2), minor_breaks = NULL) +
  guides(x = guide_axis_smart(n = 10)) +
  labs(x = NULL, y = "响应值", title = "guide_axis_smart(n = 10)") +
  theme_grey(base_size = 12)

# ---- 3. y 轴是连续轴、刻度很密时同理 ---------------------------------------

p_y <- ggplot(df, aes(x, y)) +
  geom_line(colour = "#F6903D", linewidth = 0.8) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.1)) +
  guides(y = guide_axis_smart(n = 6, nrow = 1)) +
  labs(x = "x", y = NULL, title = "连续 y 轴：n = 6") +
  theme_grey(base_size = 12)

# ---- 输出 -------------------------------------------------------------------

save_png(p_default, "06-continuous-default.png")
save_png(p_smart, "06-continuous-smart.png")
save_png(p_y, "06-continuous-y.png")

if (interactive()) print(p_smart)
