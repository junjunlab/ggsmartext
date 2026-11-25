# ============================================================================
# 分面（facet_wrap / facet_grid）下的智能标签
#
# 覆盖：
#   (1) 柱状图 facet_wrap + free_x：每个面板只标自己有的类别
#   (2) 柱状图 facet_grid + free_x：两列各自标注
#   (3) 柱状图面板内拥挤标签：自动分多行
#   (4) 热图 facet_wrap + free_y：右侧 y 轴每个面板独立标注
#
# 用法：Rscript inst/examples/10-facets.R [输出目录]
# ============================================================================

.libPaths(c(".Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggutils)
})

outdir <- if (length(commandArgs(TRUE)) >= 1L) commandArgs(TRUE)[1] else tempdir()
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

base_theme <- theme_grey(base_size = 11)

## ---- (1) facet_wrap + free_x：每个面板只标自己有的类别 --------------------
set.seed(8)
df_free <- data.frame(
  cat = factor(
    c(paste0("A", 1:15), paste0("B", 1:15), paste0("C", 1:15), paste0("D", 1:15)),
    levels = c(paste0("A", 1:15), paste0("B", 1:15),
               paste0("C", 1:15), paste0("D", 1:15))
  ),
  value = round(runif(60, 10, 100), 1),
  group = rep(c("Group A", "Group B", "Group C", "Group D"), each = 15)
)
mark <- c("A3", "A4", "A5", "A6", "B10", "C1", "C8", "D12")

p1 <- ggplot(df_free, aes(cat, value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  facet_wrap(~ group, nrow = 2, scales = "free_x") +
  guides(x = guide_axis_smart(which = mark)) +
  labs(x = NULL, y = "response",
       title = "facet_wrap + free_x: each panel labels its own categories") +
  base_theme
ragg::agg_png(file.path(outdir, "10-facets-wrap-free.png"),
        width = 10, height = 6.5, units = "in", res = 140)
print(p1)
invisible(dev.off())
message("written: 10-facets-wrap-free.png")

## ---- (2) facet_grid + free_x：两列各自标注 ---------------------------------
df_grid <- data.frame(
  cat = factor(c(paste0("g", 1:15), paste0("h", 1:15)),
               levels = c(paste0("g", 1:15), paste0("h", 1:15))),
  value = round(runif(30, 10, 100), 1),
  side = rep(c("left panel", "right panel"), each = 15)
)

p2 <- ggplot(df_grid, aes(cat, value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  facet_grid(. ~ side, scales = "free_x") +
  guides(x = guide_axis_smart(which = c("g2", "g7", "g12", "h3", "h9", "h14"))) +
  labs(x = NULL, y = "response",
       title = "facet_grid + free_x: each column labels its own categories") +
  base_theme
ragg::agg_png(file.path(outdir, "10-facets-grid-free.png"),
        width = 10, height = 5, units = "in", res = 140)
print(p2)
invisible(dev.off())
message("written: 10-facets-grid-free.png")

## ---- (3) 面板内拥挤标签：自动分多行 ---------------------------------------
df_crowd <- data.frame(
  cat = factor(paste0("c", 1:20), levels = paste0("c", 1:20)),
  value = rep(c(15, 62, 23, 78, 34, 51, 44, 29), length.out = 40),
  group = rep(c("Group A", "Group B"), each = 20)
)

p3 <- ggplot(df_crowd, aes(cat, value)) +
  geom_col(fill = "#5B8FF9", width = 0.8) +
  facet_wrap(~ group, nrow = 1) +
  guides(x = guide_axis_smart(which = 3:14, nrow = 3)) +
  labs(x = NULL, y = "response",
       title = "12 crowded labels inside a panel: spread over 3 rows") +
  base_theme
ragg::agg_png(file.path(outdir, "10-facets-crowded.png"),
        width = 10, height = 5, units = "in", res = 140)
print(p3)
invisible(dev.off())
message("written: 10-facets-crowded.png")

## ---- (4) 热图 facet_wrap + free_y：内置真实数据，每面板独立标注 -----------
data(airway_heatmap, package = "ggutils")
df_heat <- airway_heatmap$df
mark_wrap <- airway_heatmap$genes$gene[10:21]   # 顶部响应簇里连续的 12 个基因

p4 <- ggplot(df_heat, aes(sample, gene, fill = z)) +
  geom_tile() +
  facet_wrap(~ gene_set, nrow = 1, scales = "free_y") +
  scale_y_discrete(
    position = "right",
    guide = guide_axis_smart(which = mark_wrap)
  ) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, name = "z-score") +
  labs(x = NULL, y = NULL,
       title = "faceted heatmap (bundled real data): each panel labels its own genes") +
  theme_grey(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ragg::agg_png(file.path(outdir, "10-facets-heatmap.png"),
        width = 10, height = 9, units = "in", res = 140)
print(p4)
invisible(dev.off())
message("written: 10-facets-heatmap.png")
