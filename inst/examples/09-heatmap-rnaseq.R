# ============================================================================
# 用真实 GEO RNA-seq 数据演示热图基因标注（ComplexHeatmap anno_mark 风格）
#
# 数据：airway 包（Bioconductor）——GEO GSE52778
#   人气道平滑肌细胞，4 株细胞系 × 地塞米松处理/对照，共 8 个样本。
#   取变异最大的 300 个基因做竖长热图：
#     (1) 单图：右侧标注 20 个基因（顶部响应簇连续 12 个 + 散布 8 个），
#         可以清楚看到引导线长度与标签上下间距的自动分配；
#     (2) 分面图：按"处理后上调/下调"分面，每个面板数据不同、各自标注。
#
# 依赖（Suggests）：
#   if (!requireNamespace("airway", quietly = TRUE)) {
#     BiocManager::install("airway")
#   }
#
# 用法：Rscript inst/examples/09-heatmap-rnaseq.R [输出目录]
# ============================================================================

.libPaths(c(".Rlib", .libPaths()))
suppressPackageStartupMessages({
  library(ggplot2)
  library(ggsmartext)
})

if (!requireNamespace("airway", quietly = TRUE)) {
  stop(
    "需要 airway 数据包：\n",
    "  if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager')\n",
    "  BiocManager::install('airway')",
    call. = FALSE
  )
}

outdir <- if (length(commandArgs(TRUE)) >= 1L) commandArgs(TRUE)[1] else tempdir()
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

library(airway)
data(airway, package = "airway")

counts <- SummarizedExperiment::assay(airway, "counts")
cpm <- t(t(counts) / colSums(counts)) * 1e6
logcpm <- log2(cpm[rowSums(cpm > 1) >= 4, ] + 1) # expressed in >= 4 samples

## drop duplicated gene symbols so that every label is unique
name_map <- SummarizedExperiment::rowData(airway)$symbol
name_map <- ifelse(is.na(name_map) | name_map == "", rownames(airway), name_map)
names(name_map) <- rownames(airway)
dup_ids <- names(name_map)[duplicated(name_map) | duplicated(name_map, fromLast = TRUE)]
logcpm <- logcpm[!rownames(logcpm) %in% dup_ids, ]

## 300 most variable genes -> row z-scores -> hierarchical clustering
n_gene <- 300
vars <- apply(logcpm, 1, var)
z <- t(scale(t(logcpm[names(sort(vars, decreasing = TRUE))[seq_len(n_gene)], ])))
ord <- hclust(dist(z), method = "ward.D2")$order

gene_name <- name_map[rownames(z)]

## split genes into "up" / "down" after dexamethasone (used for the facets)
cd <- as.data.frame(SummarizedExperiment::colData(airway))
delta <- rowMeans(z[, cd$dex == "trt"]) - rowMeans(z[, cd$dex == "untrt"])
gene_set <- ifelse(delta > 0, "up in dexamethasone", "down in dexamethasone")

sample_lab <- factor(paste(cd$cell, cd$dex, sep = "_"),
                     levels = paste(cd$cell, cd$dex, sep = "_"))

df_gene <- data.frame(
  gene = factor(rep(gene_name[ord], times = nrow(cd)), levels = rev(gene_name[ord])),
  sample = factor(rep(sample_lab, each = nrow(z)), levels = levels(sample_lab)),
  gene_set = factor(rep(gene_set[ord], times = nrow(cd)),
                    levels = c("up in dexamethasone", "down in dexamethasone")),
  z = as.vector(z[ord, ])
)

base_theme <- theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

fill_scale <- scale_fill_gradient2(low = "#2166ac", mid = "white",
                                   high = "#b2182b", midpoint = 0, name = "z-score")

## ---- (1) tall single heatmap, anno_mark style on the right ------------------
## 20 labels: 12 neighbouring genes from the top cluster + 8 scattered ones
mark <- c(
  gene_name[ord][1:12],
  gene_name[ord][round(seq(60, n_gene - 10, length.out = 8))]
)
message("single heatmap, marked genes: ", paste(mark, collapse = ", "))

p1 <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  scale_y_discrete(
    guide = guide_none(),
    sec.axis = dup_axis(guide = guide_axis_smart(which = mark))
  ) +
  fill_scale +
  labs(x = NULL, y = NULL,
       title = paste("airway RNA-seq (GEO GSE52778):", n_gene, "genes"),
       subtitle = "20 genes labelled; leader lines show how labels are spread") +
  base_theme

f1 <- file.path(outdir, "09-heatmap-rnaseq.png")
ragg::agg_png(f1, width = 6.5 * 150, height = 10 * 150, res = 150)
print(p1)
invisible(dev.off())
message("written: ", f1)

## ---- (2) faceted heatmap: different data per panel, labels per panel -------
mark_by_set <- lapply(
  split(names(delta), gene_set),
  function(ids) ids[order(abs(delta[ids]), decreasing = TRUE)][1:4]
)
mark_facet <- unlist(mark_by_set, use.names = FALSE)
message("facet panels, marked genes: ", paste(gene_name[mark_facet], collapse = ", "))

p2 <- ggplot(df_gene, aes(x = sample, y = gene, fill = z)) +
  geom_tile() +
  facet_grid(gene_set ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(
    position = "right",
    guide = guide_axis_smart(
      which = function(labels, positions) labels %in% gene_name[mark_facet]
    )
  ) +
  fill_scale +
  labs(x = NULL, y = NULL,
       title = paste("facet by gene set,", n_gene, "genes"),
       subtitle = "each panel has different data and labels its own genes") +
  base_theme +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0))

f2 <- file.path(outdir, "09-heatmap-rnaseq-facet.png")
ragg::agg_png(f2, width = 7 * 150, height = 10 * 150, res = 150)
print(p2)
invisible(dev.off())
message("written: ", f2)
