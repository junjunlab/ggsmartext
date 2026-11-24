# ggsmartext —— 示例 08：anno_mark 风格（集中标注 / 热图基因标注）
#
# 本包的引导线几何来自 circlize 与 ComplexHeatmap::anno_mark()：
# 短垂线 -> 斜线 -> 短垂线。典型用途是"只标注少数感兴趣的类别"。
#
# 运行方式（需要先安装 ggsmartext）：
#   Rscript inst/examples/08-anno-mark.R

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

# ---- 1. 集中在一段里的标签：自动分多行 --------------------------------------

p_cluster <- ggplot(df60, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(which = 24:35)) +
  labs(x = NULL, y = "响应值", title = "which = 24:35（自动分多行）") +
  theme_grey(base_size = 12)

# ---- 2. 同样的标签强制单行：纯 anno_mark 风格 -------------------------------

p_cluster1 <- ggplot(df60, aes(x = sample, y = value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  guides(x = guide_axis_smart(which = 24:35, nrow = 1)) +
  labs(x = NULL, y = "响应值", title = "which = 24:35, nrow = 1（anno_mark 风格）") +
  theme_grey(base_size = 12)

# ---- 3. 热图基因标注：主轴隐藏 + 副轴只标 3 个基因 --------------------------

p_heat <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
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

# ---- 输出 -------------------------------------------------------------------

save_png(p_cluster, "08-cluster-auto.png")
save_png(p_cluster1, "08-cluster-nrow1.png")
save_png(p_heat, "08-heatmap-secaxis.png")

if (interactive()) print(p_heat)
