# ============================================================================
# 用包内置的真实 GEO RNA-seq 数据演示热图基因标注（ComplexHeatmap anno_mark 风格）
#
# 数据：ggutils 内置的 `airway_heatmap`（来自 Bioconductor `airway` 包，
#   GEO GSE52778：人气道平滑肌细胞，4 株细胞系 × 地塞米松处理/对照）。
#   内容是 300 个高变基因 × 8 样本的行 z-score 矩阵，已按层次聚类排好序，
#   并附带单图标注用的 20 个基因（mark）与分面用的每组 4 个基因（mark_by_set）。
#
# 生成两张图：
#   (1) 单图：右侧标注 20 个基因（顶部响应簇连续 12 个 + 散布 8 个），
#       可以看到引导线长度与标签上下间距的自动分配；
#   (2) 分面图：按"处理后上调/下调"分面，每个面板数据不同、各自标注。
#
# 用法：Rscript inst/examples/09-heatmap-rnaseq.R [输出目录]
# ============================================================================

.libPaths(c(".Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggutils)
})

outdir <- if (length(commandArgs(TRUE)) >= 1L) commandArgs(TRUE)[1] else tempdir()
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

data(airway_heatmap, package = "ggutils")
df_gene <- airway_heatmap$df
mark <- airway_heatmap$mark
mark_facet <- unlist(airway_heatmap$mark_by_set, use.names = FALSE)

base_theme <- theme_grey(base_size = 9) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

fill_scale <- scale_fill_gradient2(low = "#2166ac", mid = "white",
                                   high = "#b2182b", midpoint = 0,
                                   name = "z-score")

## ---- (1) 竖长单图：右侧 anno_mark 风格标注 20 个基因 ----------------------
message("single heatmap, marked genes: ", paste(mark, collapse = ", "))

p1 <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  scale_y_discrete(
    guide = guide_none(),
    sec.axis = dup_axis(guide = guide_axis_smart(which = mark))
  ) +
  fill_scale +
  labs(x = NULL, y = NULL,
       title = "airway RNA-seq (GEO GSE52778): 300 genes",
       subtitle = "20 genes labelled; leader lines show how labels are spread") +
  base_theme

f1 <- file.path(outdir, "09-heatmap-rnaseq.png")
ragg::agg_png(f1, width = 6.5 * 150, height = 10 * 150, res = 150)
print(p1)
invisible(dev.off())
message("written: ", f1)

## ---- (2) 分面热图：每个面板独立标注 ---------------------------------------
message("facet panels, marked genes: ",
        paste(airway_heatmap$genes$gene[match(mark_facet,
                                              airway_heatmap$genes$gene)],
              collapse = ", "))

p2 <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  facet_grid(gene_set ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(
    position = "right",
    guide = guide_axis_smart(which = mark_facet)
  ) +
  fill_scale +
  labs(x = NULL, y = NULL,
       title = "facet by gene set, 300 genes",
       subtitle = "each panel has different data and labels its own genes") +
  base_theme +
  theme_grey(base_size = 9) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0))

f2 <- file.path(outdir, "09-heatmap-rnaseq-facet.png")
ragg::agg_png(f2, width = 7 * 150, height = 10 * 150, res = 150)
print(p2)
invisible(dev.off())
message("written: ", f2)
