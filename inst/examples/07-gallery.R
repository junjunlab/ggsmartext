# ggsmartext —— 示例 07：图库生成脚本
#
# 一次性生成本包 README / vignette 用到的全套示例 PNG。
#
# 运行方式（需要先安装 ggsmartext）：
#   Rscript inst/examples/07-gallery.R            # 输出到 ./figures/
#   Rscript inst/examples/07-gallery.R out/pngs   # 输出到指定目录
#
# 每张图 1600 x 900 像素（ragg::agg_png(width = 1600, height = 900, res = 150)）。

library(ggplot2)
library(ggsmartext)

# ---- 输出目录与保存函数 -----------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("GGSMARTEXT_FIG_DIR", unset = "figures")
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
  invisible(file)
}

# ---- 数据 -------------------------------------------------------------------

set.seed(2024)
df50 <- data.frame(
  sample = factor(paste0("样本", 1:50), levels = paste0("样本", 1:50)),
  value = round(runif(50, 10, 100), 1)
)

labs40 <- paste0("样本", 1:40)
df40 <- data.frame(
  sample = factor(labs40, levels = rev(labs40)),
  value = round(runif(40, 10, 100), 1)
)

trt <- rep(c("对照组", "处理组A", "处理组B"), 8)
labs24 <- paste0("样本", 1:24, "（", trt, "）")
df24 <- data.frame(
  sample = factor(labs24, levels = labs24),
  value = round(runif(24, 10, 100), 1)
)

df_curve <- data.frame(
  x = seq(0, 100, length.out = 200),
  y = sin(seq(0, 100, length.out = 200) / 12) + rnorm(200, sd = 0.15)
)

df30 <- data.frame(
  sample = factor(paste0("样本", 1:30), levels = paste0("样本", 1:30)),
  value = round(runif(60, 10, 100), 1),
  group = rep(c("组 A", "组 B"), each = 30)
)

set.seed(5)
labs60 <- paste0("样本", 1:60)
df60 <- data.frame(
  sample = factor(labs60, levels = labs60),
  value = round(runif(60, 10, 100), 1)
)

set.seed(11)
genes <- paste0("GENE", sprintf("%03d", 1:60))
df_gene <- data.frame(
  gene = factor(genes, levels = rev(genes)),
  sample = rep(paste0("S", 1:4), each = 60),
  z = round(rnorm(240), 2)
)

# ---- 通用 x 轴条形图 --------------------------------------------------------

bar_x <- function(data, title, guide = NULL, fill = "#5B8FF9") {
  ggplot(data, aes(x = sample, y = value)) +
    geom_col(fill = fill, width = 0.8) +
    guide +
    labs(x = NULL, y = "响应值", title = title) +
    theme_grey(base_size = 12)
}

# ---- 1. 痛点：默认轴 --------------------------------------------------------

p <- bar_x(df50, "默认轴：50 个标签挤成一团")
save_png(p, "gallery-01-default.png")

# ---- 2. 快速开始 ------------------------------------------------------------

p <- bar_x(df50, "guide_axis_smart(n = 10, nrow = 2)",
           guides(x = guide_axis_smart(n = 10, nrow = 2)))
save_png(p, "gallery-02-basic.png")

# ---- 3. which 指定子集 ------------------------------------------------------

p <- bar_x(df50, "which = seq(1, 50, by = 10)",
           guides(x = guide_axis_smart(which = seq(1, 50, by = 10), nrow = 2)))
save_png(p, "gallery-03-which-integer.png")

p <- bar_x(df50, 'which = grepl("0$", levels(sample))',
           guides(x = guide_axis_smart(which = grepl("0$", levels(df50$sample)), nrow = 1)))
save_png(p, "gallery-04-which-logical.png")

# ---- 4. n 自动挑：少 vs 多 --------------------------------------------------

p <- bar_x(df50, "n = 5（自动等距挑 5 个）",
           guides(x = guide_axis_smart(n = 5, nrow = 1)))
save_png(p, "gallery-05-n5.png")

p <- bar_x(df50, "n = 20, nrow = 2",
           guides(x = guide_axis_smart(n = 20, nrow = 2)))
save_png(p, "gallery-06-n20.png")

# ---- 5. 多行：三行 ----------------------------------------------------------

p <- bar_x(df50, "n = 24, nrow = 3",
           guides(x = guide_axis_smart(n = 24, nrow = 3)))
save_png(p, "gallery-07-nrow3.png")

# ---- 6. 引导线样式 ----------------------------------------------------------

p <- bar_x(df50, 'connection_style = "diagonal"（默认）',
           guides(x = guide_axis_smart(n = 12, nrow = 2, connection_style = "diagonal")))
save_png(p, "gallery-08-diagonal.png")

p <- bar_x(df50, 'connection_style = "straight"',
           guides(x = guide_axis_smart(n = 12, nrow = 2, connection_style = "straight")))
save_png(p, "gallery-09-straight.png")

p <- bar_x(df50, 'connection_style = "elbow"',
           guides(x = guide_axis_smart(n = 12, nrow = 2, connection_style = "elbow")))
save_png(p, "gallery-10-elbow.png")

# ---- 7. 刻度线：tick = "shown" 只给显示出来的标签画刻度 ---------------------

p <- bar_x(df50, 'tick = "shown"：只给显示的标签画刻度',
           guides(x = guide_axis_smart(n = 10, nrow = 2, tick = "shown")))
save_png(p, "gallery-11-tick-shown.png")

# ---- 8. y 轴：nrow 变成列数 -------------------------------------------------

p <- ggplot(df40, aes(x = value, y = sample)) +
  geom_col(fill = "#F6903D", width = 0.8) +
  guides(y = guide_axis_smart(n = 10, nrow = 2)) +
  labs(x = "响应值", y = NULL, title = "y 轴：n = 10, nrow = 2（2 列）") +
  theme_grey(base_size = 12)
save_png(p, "gallery-12-y-axis.png")

# ---- 9. 长标签：多行 vs 45 度 -------------------------------------------------

p <- bar_x(df24, "长标签多行错开：n = 12, nrow = 2",
           guides(x = guide_axis_smart(n = 12, nrow = 2)))
save_png(p, "gallery-13-long-rows.png")

p <- bar_x(df24, "angle = 45, hjust = 1, vjust = 1",
           guides(x = guide_axis_smart(n = 12, nrow = 1, angle = 45, hjust = 1, vjust = 1)))
save_png(p, "gallery-14-angle-45.png")

p <- bar_x(df24, "angle = 90, hjust = 1, vjust = 0.5",
           guides(x = guide_axis_smart(n = 12, nrow = 1, angle = 90, hjust = 1, vjust = 0.5)))
save_png(p, "gallery-15-angle-90.png")

# ---- 10. 连续轴 -------------------------------------------------------------

p <- ggplot(df_curve, aes(x, y)) +
  geom_line(colour = "#5B8FF9", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 100, by = 2), minor_breaks = NULL) +
  guides(x = guide_axis_smart(n = 10, nrow = 2)) +
  labs(x = NULL, y = "响应值", title = "连续轴：51 个刻度，只显示 10 个") +
  theme_grey(base_size = 12)
save_png(p, "gallery-16-continuous.png")

# ---- 11. facet：每个面板各自排布 --------------------------------------------

p <- ggplot(df30, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  facet_wrap(~group, scales = "free_x") +
  guides(x = guide_axis_smart(n = 6, nrow = 2)) +
  labs(x = NULL, y = "响应值", title = "facet_wrap：每个面板独立排布") +
  theme_grey(base_size = 12)
save_png(p, "gallery-17-facet.png")

# ---- 12. coord_flip：x 轴标签跑到左边；guide 要挂在 scale 上 ----------------

p <- bar_x(df50, "coord_flip 之后：x 轴标签变成竖直轴") +
  scale_x_discrete(guide = guide_axis_smart(n = 10, nrow = 2)) +
  coord_flip()
save_png(p, "gallery-18-coord-flip.png")

# ---- 13. 与 theme 配合：文本样式从主题继承 ---------------------------------

p <- bar_x(df50, "theme_grey(base_size = 13) + 灰色标签",
           guides(x = guide_axis_smart(n = 12, nrow = 2))) +
  theme(axis.text.x = element_text(colour = "grey30", size = 9))
save_png(p, "gallery-19-theme.png")

# ---- 14. anno_mark 风格：集中标注（自动分行 / 单行对照） --------------------

p <- ggplot(df60, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(which = 24:35)) +
  labs(x = NULL, y = "响应值", title = "which = 24:35（自动分多行）") +
  theme_grey(base_size = 12)
save_png(p, "gallery-20-cluster-auto.png")

p <- ggplot(df60, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(which = 24:35, nrow = 1)) +
  labs(x = NULL, y = "响应值", title = "which = 24:35, nrow = 1（anno_mark 风格）") +
  theme_grey(base_size = 12)
save_png(p, "gallery-21-cluster-nrow1.png")

# ---- 15. 热图基因标注：主轴隐藏 + 副轴只标 3 个基因 --------------------------

p <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  scale_y_discrete(
    guide = guide_none(),
    sec.axis = dup_axis(
      guide = guide_axis_smart(which = c("GENE005", "GENE018", "GENE047"))
    )
  ) +
  scale_fill_gradient2(low = "#2E5EAA", mid = "white", high = "#C0392B") +
  labs(x = NULL, y = NULL, title = "热图基因注释：只标 3 个基因") +
  theme_grey(base_size = 12)
save_png(p, "gallery-22-heatmap-secaxis.png")

# ---- 汇总 -------------------------------------------------------------------

files <- list.files(out_dir, pattern = "^gallery-.*\\.png$", full.names = TRUE)
message(length(files), " 张图已写入 ", normalizePath(out_dir, mustWork = FALSE))
invisible(files)
